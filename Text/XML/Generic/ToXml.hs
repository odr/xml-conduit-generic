{-# LANGUAGE FlexibleInstances, TypeOperators, DefaultSignatures, DeriveGeneric, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
-- |
-- Module: Text.XML.Generic.ToXml
-- Copyright: 2013 Dmitry Olshansky
-- License: BSD3
--
-- Maintainer: olshanskydr@gmail.com
--
-- Define class 'ToXml' to transform ADT to xml. Generic instances could be used.
--
-- E.g.
--
-- > {-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
-- > ...
-- > data A = A1 { aText :: T.Text, aInt :: Int } | A2 { aText :: T.Text } deriving Generic
-- > instance ToXml A
-- > data B = B { bText :: T.Text, bAs :: [A] } deriving Generic
-- > instance ToXml B
--
-- >>> runToXml $ B "btext" [A1 "a1text" 5, A2 "a2text"]
-- Just "<B bText=\"btext\"><bAs><A1 aText=\"a1text\" aInt=\"5\"/></bAs><bAs><A2 aText=\"a2text\"/></bAs></B>"
--
-- The generation rules are controlled by parameters in 'TOX' argument of 'toXml' ('runToXml' use default settings).
-- Default rules are:
--
-- * Make attribute if possible
--
-- * To get an element name use selector (field) name if provided
--
-- * If there is no selector use datatype name 
--
-- * For sum types make additional element with name of constructor
--
module Text.XML.Generic.ToXml (ToXml(..), runToXml
        , TOX(..), TO(..), TOF(..)
    ) where

--
--
import Text.XML.Generic.ToXmlUtil

import Control.Arrow
import Control.Monad(when)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Default(Default(..))
import Data.List(intercalate)
import qualified Data.Map as M
import Data.Maybe -- (catMaybes, fromMaybe, isNothing, maybeToList)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.XML.Types(Event(..), Content(..), Name(..))
import GHC.Generics
import qualified Text.XML.Stream.Render as XR

showT :: (Show a) => a -> T.Text
showT = T.pack . show

{-
data A = A1 { aText :: T.Text, aInt :: Int } | A2 { aText :: T.Text } deriving Generic
instance ToXml A
data B = B { bText :: T.Text, bAs :: [A] } deriving Generic
instance ToXml B
-}
-- | Provide conduit to convert type to xml-events
class ToXml a where 
    toXml       :: Monad m  
                => TOX                  -- ^ parameters of transformation  
                -> Conduit a m Event    -- ^ conduit from some types to XML-events
    asAttr      :: TOX                  -- ^ parameters of transformation
                -> a                    -- ^ 
                -> Maybe (Name, [Content])  -- ^ transform type to attribute if possible
    attrs       :: TOX                  -- ^ parameters of transformation
                -> a 
                -> [(Name, [Content])]  -- ^ list of children attributes
    default toXml   :: (Generic a,  GToXml (Rep a), Monad m) => TOX -> Conduit a m Event
    toXml tox    
        | toRoot $ toxTo tox = CL.map (M1 . K1) =$= gToXmlNoSel tox
        | otherwise = CL.map from =$= gToXml tox
    default asAttr      :: (Generic a, GToXml (Rep a)) => TOX -> a -> Maybe (Name, [Content])
    asAttr tox   = gAsAttr tox . from
    default attrs       :: (Generic a, GToXml (Rep a)) => TOX -> a -> [(Name, [Content])]
    attrs tox    = gAttrs tox . from
   
-- | utitlity function to serialize single value to xml with default settings
runToXml :: (Monad m, Functor m, ToXml a, MonadUnsafeIO m, MonadThrow m) => a -> m (Maybe T.Text)
runToXml a = fmap (listToMaybe) $ CL.sourceList [a] $$ toXml (TOX def def {toRoot = True}) =$ XR.renderText def =$ CL.consume 

instance ToXml T.Text where
    toXml _     = CL.filter (not.T.null) =$= CL.map (EventContent . ContentText)
    asAttr tox t 
        | T.null t  = Nothing
        | otherwise = fmap (flip (,) [ContentText t]) $ getAttrName tox
    attrs _     = const []

instance ToXml Integer where
    toXml tox   = CL.map showT =$= toXml tox
    asAttr t    = asAttr t . showT
    attrs _     = const []

instance ToXml Int where
    toXml tox   = CL.map showT =$= toXml tox
    asAttr t    = asAttr t . showT
    attrs _     = const []

instance ToXml Double where
    toXml tox   = CL.map showT =$= toXml tox
    asAttr t    = asAttr t . showT
    attrs _     = const []

instance ToXml Float where
    toXml tox   = CL.map showT =$= toXml tox
    asAttr t    = asAttr t . showT
    attrs _     = const []

instance ToXml Bool where
    toXml tox       = CL.map (\b1->if b1 then "true" else "false" :: T.Text) =$= toXml tox
    asAttr t True   = asAttr t ("true" :: T.Text)
    asAttr t False  = asAttr t ("false" :: T.Text)
    attrs _         = const []

instance (ToXml a) => ToXml [a] where
    toXml tox       = CL.concatMap id =$= toXml tox
    asAttr _ []     = Nothing
    asAttr t x      = fmap ((head *** intercalate [ContentText " "]) . unzip) $ mapM (asAttr t) x
    attrs t         = maybeToList . asAttr t

instance (ToXml a) => ToXml (Maybe a) where
    toXml tox   = CL.catMaybes =$= toXml tox
    asAttr t    = (>>= asAttr t)
    attrs t     = maybe [] $ attrs t

data Pair a b = Pair { fstVal :: a, sndVal :: b } deriving Generic
instance (ToXml a, ToXml b) => ToXml (Pair a b)


instance (ToXml a, ToXml b) => ToXml (a,b) 
  where
    toXml = mapInput (uncurry Pair) (Just . (fstVal &&& sndVal)) . toXml
    asAttr t = asAttr t . uncurry Pair
    attrs t = attrs t . uncurry Pair

instance (ToXml k, Ord k, ToXml v) => ToXml (M.Map k v) where
    toXml = mapInput M.toList (Just . M.fromList) . toXml 
    asAttr t = asAttr t . M.toList
    attrs t = attrs t . M.toList

instance (ToXml a, Ord a) => ToXml (S.Set a) where
    toXml = mapInput S.toList (Just . S.fromList) . toXml 
    asAttr t = asAttr t . S.toList
    attrs t = attrs t . S.toList


class GToXml f where
    gToXml      :: Monad m => TOX -> Conduit (f a) m Event
    gAsAttr     :: TOX -> f a -> Maybe (Name, [Content])
    gAttrs      :: TOX -> f a -> [(Name, [Content])] -- ^ children attrs

instance GToXml U1 where
    gToXml _    =  return ()
                 --  yield (M1 $ K1 mempty :: M1 S NoSelector (K1 R T.Text) a) =$= gToXml to
    gAsAttr _   = const Nothing
    gAttrs _ _  = []

instance (GToXml a, Selector c) => GToXml (M1 S c a) where
-- selector (i.e. fieldName) 
    gToXml = gToXmlSel
    gAsAttr tox mi@(M1 a)   = gAsAttr (toxSel tox mi) a
    gAttrs tox mi@(M1 a)    = gAttrs (toxSel tox mi) a

gToXmlSel :: (Monad m, Selector c, GToXml x) => TOX -> Conduit (M1 S c x a) m Event
gToXmlSel tox  = -- trace "sel" $ 
                  awaitForever
               $ \mi@(M1 x) -> doTo (toxSel tox mi) gAsAttr gAttrs mi
                                    (yield x =$= gToXml (toxSel tox mi))
gToXmlNoSel :: (Monad m, ToXml x) => TOX -> Conduit (M1 S NoSelector (K1 s x) x) m Event
gToXmlNoSel = gToXmlSel

instance (GToXml a, Datatype c) => GToXml (M1 D c a) where
-- datatype
    gToXml tox = -- trace "dt" $ 
                 awaitForever 
               $ \mi@(M1 x) -> doTo (toxDT tox mi) gAsAttr gAttrs mi
                                    (yield x =$= gToXml (toxDT tox mi))
 
    gAsAttr tox mi@(M1 a)   = gAsAttr (toxDT tox mi) a
    gAttrs tox mi@(M1 a)    = gAttrs (toxDT tox mi) a

instance (GToXml a, GToXml b) => GToXml (a :+: b) where
    gToXml tox = awaitForever $ \ab -> 
                    case ab of
                        L1 a -> doTo' a
                        R1 b -> doTo' b        
        where
            doTo' x = doTo (toxSum tox) (\_ _ -> Nothing) (\_ _ -> []) x
                            (yield x =$= gToXml (toxSum tox))

    gAsAttr _   = const Nothing
    gAttrs _ _  = [] 

instance (GToXml a, Constructor c) => GToXml (M1 C c a) where
-- constructor
    gToXml tox  = awaitForever
                $ \mi@(M1 x) -> doTo (toxCon tox mi) gAsAttr gAttrs mi
                                    (yield x =$= gToXml (toxCon tox mi))
    gAsAttr _ _ = Nothing
    gAttrs tox mi@(M1 a)    = gAttrs (toxCon tox mi) a

instance (GToXml a, GToXml b) => GToXml (a :*: b) where
    gToXml tox = awaitForever $ \(a:*:b) -> do
        let (ma, mb) = (gAsAttr tox a, gAsAttr tox b)
        when (isNothing ma) $ yield a =$= gToXml tox
        when (isNothing mb) $ yield b =$= gToXml tox
    gAsAttr _               = const Nothing
    gAttrs tox (a :*: b)    = gAttrs tox a ++ gAttrs tox b

instance (ToXml a) => GToXml (K1 i a) where
    -- value
    gToXml tox = CL.map unK1 =$= toXml (toxNotRoot tox) 
    gAsAttr tox (K1 a) = asAttr (toxNotRoot tox) a
    gAttrs tox (K1 a) = maybeToList $ asAttr (toxNotRoot tox) a

