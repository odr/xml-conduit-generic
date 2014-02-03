{-# LANGUAGE FlexibleInstances, TypeOperators, DefaultSignatures, DeriveGeneric, FlexibleContexts, OverloadedStrings, RankNTypes  #-} --, OverlappingInstances#-}
-- |
-- Module: Text.XML.Generic.ToXml
-- Copyright: 2013 Dmitry Olshansky
-- License: BSD3
--
-- Maintainer: olshanskydr@gmail.com
--
-- Define class FromXml to transform xml to ADT. Generic instances could be used.
--
module Text.XML.Generic.FromXml (FromXml(..), runFromXml, linearize) where -- , runGFromXml) where

import Text.XML.Generic.FromXmlUtil

import Control.Applicative
import Control.Arrow((&&&))
import Control.Monad(liftM2)
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.State
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Default(Default(..))
import qualified Data.Text as T
import Data.Time
import Data.XML.Types(Event(..), Content(..))
import GHC.Generics
import Safe
import Text.Printf(printf)
import qualified Text.XML.Stream.Parse as XP
import qualified Data.Map as M
import qualified Data.Set as S
import System.Locale(defaultTimeLocale)

-- | Utility function to convert xml from text to Haskell-ADT 
runFromXml :: (Monad m, MonadThrow m, Functor m, FromXml a) 
            => T.Text -> m (Either String a)
runFromXml t = CL.sourceList [t] $$ XP.parseText def =$ CL.map snd =$ runFromXml'

runFromXml' :: (Monad m, Functor m, FromXml a) => Sink Event m (Either String a)
runFromXml' = linearize =$ transPipe (flip evalStateT def) (fromXml def)

-- fromXml works only with linearized events (only Begin/End/ContentText).

-- | Provide consumer from xml-event stream to ADT
class FromXml a where
    fromXml :: (Monad m, Functor m) 
            => FO                                       -- ^ Parameters of transformation
            -> Consumer Event (Mon m) (Either String a) -- ^ Consumer with result in (Either String) monad
    default fromXml :: (Generic a, GFromXml (Rep a), Monad m, Functor m) 
                    => FO -> Consumer Event (Mon m) (Either String a)
    fromXml fo
        | foRoot fo = fmap (fmap $ unK1 . unM1) $ gFromXmlNoSel fo { foRoot = False }
        | otherwise = fmap (fmap to) $ gFromXml fo

err :: String -> String -> String
err s e = printf "Error in FromXml instance for '%s'. Can't convert '%s' to '%s'" s e s
   
instance FromXml T.Text where
    fromXml fo = fmap Right $ do
        mt <- awaitOpt fo
        case mt of
            Nothing -> return ""
            Just (EventContent (ContentText t)) -> return t
            Just x -> leftoverOpt x >> return ""

{-
instance FromXml [Char] where
    fromXml = fmap (fmap T.unpack) . fromXml
-}

fromXmlReadF :: (Monad m, Functor m) => (String -> Maybe a) -> String -> FO 
                                     -> Consumer Event (Mon m) (Either String a)
fromXmlReadF f t fo = do
    mt <- awaitOpt fo
    case mt of
        Nothing -> return $ Left $ err t $ show mt
        Just x@(EventContent (ContentText txt)) -> case T.words txt of 
            [] -> leftoverOpt x >> return (Left $ err t $ show mt) 
            (z:[]) -> let s = T.unpack z in return $ maybe (Left $ err t s) Right $ f s 
            (z:zs) -> do
                let s = T.unpack z
                leftoverOpt $ EventContent $ ContentText $ T.unwords zs  
                return $ maybe (Left $ err t s) Right $ f s 
        Just x -> leftoverOpt x >> return (Left $ err t $ show mt)

fromXmlRead :: (Monad m, Functor m, Read a) => String -> FO -> Consumer Event (Mon m) (Either String a)
fromXmlRead = fromXmlReadF readMay 

instance FromXml Integer where
    fromXml = fromXmlRead "Integer"
                
instance FromXml Int where
    fromXml = fromXmlRead "Int"
                
instance FromXml Double where
    fromXml = fromXmlRead "Double"

instance FromXml Float where
    fromXml = fromXmlRead "Float"

instance FromXml Bool where
    fromXml = fromXmlReadF readBool "Bool"
      where
        readBool s  | s == "true"   = Just True
                    | s == "false"  = Just False
                    | otherwise     = Nothing 

fromXmlReadTime :: (Monad m, Functor m, ParseTime t) => String -> FO -> Consumer Event (Mon m) (Either String t)
fromXmlReadTime = fromXmlReadF (parseTime defaultTimeLocale "%FT%T")

instance FromXml UTCTime where
    fromXml =  fromXmlReadTime "UTCTime"

instance FromXml LocalTime where
    fromXml =  fromXmlReadTime "LocalTime"

instance (FromXml a) => FromXml (Maybe a) where
    fromXml fo = do
        xs <- lift $ getAndModifyEvents $ const []
        r <- fmap (either (const Nothing) Just) $ fromXml fo { foOpt = True }
        case (foOpt fo, r) of
            (False, Just _) -> return ()
            (True, Just _)  -> lift $ modifyEvents (++xs)
            (_, Nothing)    -> do
                    s <- lift get
                    mapM_ leftover $ fsEvents s
                    lift $ put s { fsEvents = xs } 
        return $ Right r

instance (FromXml a) => FromXml [a] where
    fromXml fo = go id
      where
        go front = do
            ema <- fromXml fo
            case ema of
                (Right (Just a))    -> go $ front . (a:)
                _                   -> return $ Right $ front []

data Pair a b = Pair { fstVal :: a, sndVal :: b } deriving Generic
instance (FromXml a, FromXml b) => FromXml (Pair a b)

instance (FromXml a, FromXml b) => FromXml (a,b) 
  where
    fromXml = fmap (fmap $ fstVal &&& sndVal) . fromXml

instance (FromXml k, Ord k, FromXml v) => FromXml (M.Map k v) where
    fromXml = fmap (fmap M.fromList) . fromXml 

instance (FromXml a, Ord a) => FromXml (S.Set a) where
    fromXml = fmap (fmap S.fromList) . fromXml 

class GFromXml f where
    gFromXml :: (Monad m, Functor m) => FO -> Consumer Event (Mon m) (Either String (f a))

instance GFromXml U1 where
    gFromXml _ = return $ Right U1

instance (GFromXml a, GFromXml b) => GFromXml (a :*: b) where
    gFromXml fo = do
        lift $ modifyIsProd $ const True
        liftM2 (liftM2 (:*:)) (gFromXml fo) (gFromXml fo)

            
instance (GFromXml a, GFromXml b) => GFromXml (a :+: b) where
    -- check first as Maybe - to clear Event-stack.
    -- Error (Left) will be only in the last constructor.
    gFromXml fo = do
        lift $ modifyIsSum $ const True
        xs <- lift $ getAndModifyEvents $ const []
        er <- gFromXml fo { foOpt = True }
        case (foOpt fo, er) of
            (False, Right r)    -> return $ Right $ L1 r
            (True, Right r)     -> lift (modifyEvents (++xs)) >> return (Right $ L1 r)
            (_, Left _)         -> do
                    s <- lift get
                    mapM_ leftover $ fsEvents s
                    lift $ put s { fsEvents = xs }
                    fmap (fmap R1) $ gFromXml fo

instance (GFromXml a, Datatype c) => GFromXml (M1 D c a) where
    gFromXml fo = doFrom fo getRes fmod $ foDt fo
        where
            getRes = fmap (fmap M1) . gFromXml
            -- fmod :: (Monad m, Datatype c) => M1 D c a x -> Mon m ()
            fmod = modifyDT . const . Just . ((,) <$> datatypeName <*> moduleName)

instance (GFromXml a, Constructor c) => GFromXml (M1 C c a) where
-- constructor
    gFromXml fo = doFrom fo getRes fmod $ foCons fo
        where
            getRes = fmap (fmap M1) . gFromXml
            fmod = modifyCons . const . Just 
                                . ((,) <$> conName 
                                       <*> ((,) <$> conFixity <*> conIsRecord))

gFromXmlNoSel   :: (Monad m, Functor m, FromXml x) 
                => FO -> Consumer Event (Mon m) (Either String (M1 S NoSelector (K1 s x) x))
gFromXmlNoSel = gFromXmlSel

gFromXmlSel :: (Monad m, Functor m, Selector c, GFromXml x) 
            => FO -> Consumer Event (Mon m) (Either String (M1 S c x a))
gFromXmlSel fo = doFrom fo getRes fmod $ foSel fo
    where
        getRes = fmap (fmap M1) . gFromXml
        fmod = modifySel . const . Just . selName 

instance (GFromXml (K1 x a), Selector c) => GFromXml (M1 S c (K1 x a)) where
-- selector (i.e. fieldName) 
    gFromXml = gFromXmlSel

instance (Selector c, GFromXml (K1 R (Maybe a))) 
        => GFromXml (M1 S c (K1 R (Maybe a))) where
-- selector (i.e. fieldName) 
    gFromXml fo = do
        xs <- lift $ getAndModifyEvents $ const []
        mr <- fmap (either (const $ M1 $ K1 Nothing) id) 
            $ gFromXmlSel fo { foOpt = True }

        case (foOpt fo, unK1 $ unM1 mr) of
            (False, Just _) -> return ()
            (True, Just _)  -> lift $ modifyEvents (++xs)
            (_, Nothing)    -> do
                    s <- lift get
                    mapM_ leftover $ fsEvents s
                    lift $ put s { fsEvents = xs } 
        return $ Right mr

instance (FromXml a) => GFromXml (K1 i a) where
    gFromXml fo = fmap (fmap K1) $ fromXml fo
