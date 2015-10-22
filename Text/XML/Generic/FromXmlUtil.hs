{-# LANGUAGE FlexibleInstances, TypeOperators, DefaultSignatures, DeriveGeneric, FlexibleContexts, OverloadedStrings, RankNTypes, EmptyDataDecls #-}
-- |
-- Module: Text.XML.Generic.ToXml
-- Copyright: 2013 Dmitry Olshansky
-- License: BSD3
--
-- Maintainer: olshanskydr@gmail.com
--
-- Parameters of xml to ADT conversion and some utils for FromXml
--
module Text.XML.Generic.FromXmlUtil where

import Control.Arrow
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.State
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Default(Default(..))
import qualified Data.Text as T
import Data.XML.Types(Event(..), Content(..), Name(..))
import GHC.Generics
import Text.Printf(printf)
import Data.Monoid ((<>))

{-
import Control.Monad(when)
import Debug.Trace 
-- _trace = False
_trace = True
trace' s = when _trace $ trace s $ return ()
-}

-- | Transformation works in State monad with state 'FS'
type Mon m = StateT FS m

-- | Label for Element
data E
-- | Label for Attribute
data A


-- | Two functions which are used in conversion process for attributes and elements.
-- Type parameters are labels (S | D | C) and (E | A)
data Checks s t = Checks    { chIs :: forall m. Monad m => Mon m Bool -- ^ should we try to convert attribute (or element) to Selector (or Datatype or Constructor)
                            , chCheck :: forall m. Monad m => Mon m Bool  -- whether conversion successful, i.e. names are equivalent
                            }

-- | Checks for element and attribute 
data ChecksEA s = ChecksEA  { ceaElem :: Checks s E     -- ^ Checks for element
                            , ceaAttr :: Checks s A     -- ^ Checks for attribute
                            }

-- | Parameters of conversion
data FO = FO    { foOpt     :: Bool         -- ^ Is optional conversion (for Maybe etc)
                , foRoot    :: Bool         -- ^ Is root element. True only on very begining
                , foSel     :: ChecksEA S   -- ^ Functions to check selector
                , foDt      :: ChecksEA D   -- ^ Functions to check datatype
                , foCons    :: ChecksEA C   -- ^ Functions to check constructor
                }

-- it works in sequence:
--  Enter Selector => Enter Datatype => Enter Constructor => Enter K1 => Val 
--      => Exit K1 => Exit Constructor => Exit Datatype => Exit Selector

instance Default FO where 
    def = FO    { foOpt     = False 
                , foRoot    = True 
                , foSel     = chSelEA 
                , foDt      = chDtEA 
                , foCons    = chConsEA
                }
        where
            getElem :: FS -> Name
            getElem = maybe "?" fst . fsElem
            getAttr :: FS -> Name
            getAttr = maybe "?" fst  . fsAttr

            chSelEA :: ChecksEA S
            chSelEA = ChecksEA  { ceaElem = check getElem
                                , ceaAttr = check getAttr }
                where
                    check f = Checks    { chIs  = return True
                                        , chCheck = do
                                            (s,iss,d,c,e) <- gets $ (,,,,) 
                                                <$> maybe "" T.pack . fsSel 
                                                <*> fsIsSum 
                                                <*> maybe "" (T.pack . fst) . fsDT
                                                <*> maybe "" (T.pack . fst) . fsCons 
                                                <*> nameLocalName . f
                                            return  $  (not (T.null s) && e == s)
                                                    || (T.null s && iss && e == d)
                                                    || (T.null s && not iss && T.null c && e == d)
                                                    || (T.null s && not iss && not (T.null c) && e == c)
                                        }

            chDtEA :: ChecksEA D
            chDtEA = ChecksEA   { ceaElem = check getElem
                                , ceaAttr = check getAttr }
                where
                    check f = Checks    { chIs      = return False
                                        , chCheck   = do -- not used if chIs == return False
                                            gets $ (==) 
                                                <$> maybe "" (T.pack . fst) . fsDT 
                                                <*> nameLocalName . f
                                        }

            chConsEA :: ChecksEA C
            chConsEA = ChecksEA { ceaElem = check getElem
                                , ceaAttr = check getAttr }
                where
                    check f = Checks    { chIs  = gets fsIsSum
                                        , chCheck = do
                                            gets $ (==) 
                                                <$> maybe "" (T.pack  . fst) . fsCons
                                                <*> nameLocalName . f
                                        }
 
-- | State of conversion
data FS = FS    { fsEvents  :: [Event]                  -- ^ stack of Events to leftover them 
                                                        --   in case of parse Nothing
                , fsElem    :: Maybe (Name, [(Name, [Content])])    
                                                        -- ^ current Elem (elemName, attrs)
                , fsAttr    :: Maybe (Name, [Content])  -- ^ current Attr to check
                , fsCons    :: Maybe (String, (Fixity, Bool)) -- ^ current constructor to check
                , fsDT      :: Maybe (String, String)   -- ^ current datatype to check
                , fsSel     :: Maybe String             -- ^ current selector to check
                , fsIsSum   :: Bool                     -- ^ does Sum type there
                , fsIsProd  :: Bool                     -- ^ does we have Product constructor
                } deriving Show

instance Default FS where
    def = FS def def def def def def False False

getAndModifyEvents  :: (Monad m, Functor m)
                    => ([Event] -> [Event]) -> StateT FS m [Event]
getAndModifyEvents f = gets fsEvents <* modifyEvents f

modifySel :: Monad m =>  (Maybe String -> Maybe String) -> Mon m () 
modifySel f     = modify $ \fs -> fs { fsSel    = f (fsSel fs) }

modifyEvents    :: Monad m
                => ([Event] -> [Event]) -> StateT FS m ()
modifyEvents f  = modify $ \fs -> fs { fsEvents = f (fsEvents fs) }

modifyElem  :: Monad m 
            => (Maybe (Name, [(Name, [Content])]) -> Maybe (Name, [(Name, [Content])]))
            -> StateT FS m ()
modifyElem f    = modify $ \fs -> fs { fsElem   = f (fsElem fs) }

modifyAttr  :: Monad m 
            => (Maybe (Name, [Content]) -> Maybe (Name, [Content]))
            -> StateT FS m ()
modifyAttr f    = modify $ \fs -> fs { fsAttr   = f (fsAttr fs) }

modifyCons  :: Monad m 
            => (Maybe (String, (Fixity, Bool)) -> Maybe (String, (Fixity, Bool)))
            -> StateT FS m ()
modifyCons f    = modify $ \fs -> fs { fsCons   = f (fsCons fs) }

modifyDT    :: Monad m 
            => (Maybe (String, String) -> Maybe (String, String))
            -> StateT FS m ()
modifyDT f      = modify $ \fs -> fs { fsDT     = f (fsDT fs) }

modifyIsSum :: Monad m 
            => (Bool -> Bool) -> StateT FS m ()
modifyIsSum f   = modify $ \fs -> fs { fsIsSum  = f (fsIsSum fs) }

modifyIsProd    :: Monad m 
                => (Bool -> Bool) -> StateT FS m ()
modifyIsProd f  = modify $ \fs -> fs { fsIsProd = f (fsIsProd fs) }

await'  :: Monad m => Consumer a (Mon m) (Maybe a)
await'      = lift get >>= (await <*) . lift . put
leftover'   :: Monad m => a -> Conduit a (Mon m) b
leftover' x = lift get >>= (leftover x <*) . lift . put

-- | Linearize xml. I.e. 
--
-- * left only EventBeginElement / EventEndElement / EventContent
--
-- * concat content to one ContentText
--
linearize   :: Monad m => Conduit Event m Event
linearize   = awaitForever $ \e -> do
    case e of
        EventBeginElement _ _   -> yield e
        EventEndElement _       -> yield e
        EventContent c          -> go (getText c <>) >>= yield . EventContent . ContentText
        _                       -> return ()
    where
        getText (ContentText t) = t
        getText (ContentEntity t) = t
        go front = do
            mx <- await
            case mx of
                Just (EventContent c)   -> go $ front . (getText c <>)
                Just x                  -> leftover x >> return (front mempty)
                Nothing                 -> return $ front mempty

awaitOpt :: (Monad m, Functor m) => FO -> Consumer Event (Mon m) (Maybe Event)
awaitOpt fo 
    | not $ foOpt fo    = await'
    | otherwise         = do
            ma <- await'
            maybe (return ()) 
                  (\a -> lift $ modifyEvents (a:)) 
                  ma
            return ma

leftoverOpt :: (Monad m, Functor m) => a -> Conduit a (Mon m) o
leftoverOpt x = do
    leftover' x
    lift $ modifyEvents $ drop 1

doFrom :: (Monad m, Functor m)
    => FO -> (FO -> Consumer Event (Mon m) (Either String x)) -> (x -> Mon m ()) -> ChecksEA t
                -> Consumer Event (Mon m) (Either String x)
doFrom fo getRes fmod chs = do
    isA <- lift $ chIs $ ceaAttr chs
    if isA 
        then do -- process attrs from parent elem
            mba <- lift $ checkAttrs (getRes def) fmod $ chCheck $ ceaAttr chs
            maybe (do
                    isE <- lift $ chIs $ ceaElem chs
                    if isE 
                        then withEl fo getRes fmod $ chCheck $ ceaElem chs -- process new elem
                        else return $ Left "There is no attr correspondence and elem is prohibited" 
                )
                (return . Right) mba
        else do
            isE <- lift $ chIs $ ceaElem chs
            if isE 
                then withEl fo getRes fmod $ chCheck $ ceaElem chs -- process new elem
                else withoutEl fo getRes fmod 

withEl :: (Monad m, Functor m)
        => FO -> (FO -> Consumer Event (Mon m) (Either String x)) -> (x -> Mon m ()) -> Mon m Bool 
                -> Consumer Event (Mon m) (Either String x)
withEl fo getRes fmod checkEl = do
    me <- awaitOpt fo
    case me of 
        Just (EventBeginElement e attrs) -> do
            parentEl <- lift $ gets fsElem <* modifyElem (const $ Just (e,attrs))
            er <- getRes fo
            res <- either (return.Left) (\r -> do
                    lift $ fmod r
                    meEnd <- awaitOpt fo
                    case meEnd of
                        Just (EventEndElement e') 
                            | e /= e'   -> return $ Left $ printf (unlines 
                                    [ "Invalid end of element."
                                    , "Expected end of: '%s'."
                                    , "Got end of '%s'" ]) 
                                (show e) (show e')
                            | otherwise -> do
                                checked <- lift $ checkEl
                                if checked then return $ Right r
                                    else do
                                    fs <- lift get    
                                    return $ Left $ printf (unlines
                                        [ "Element doesn't correspond value."
                                        , "Element: '%s'"
                                        , "Current state: '%s'" ]
                                        ) (show e) (show fs) 
                        e' -> return $ Left $ printf
                            "Invalid end of element in constructor.\nExpected end of '%s'.\nGot event '%s'" 
                            (show e) (show e')
                ) er
            lift $ modifyElem $ const parentEl
            return res
        e' -> return $ Left $ printf
                "Begin of element was expected.\n But got an event '%s'"
                (show e')
  
withoutEl :: (Monad m, Functor m)
        => FO -> (FO -> Consumer Event (Mon m) (Either String x)) -> (x -> Mon m ())
                -> Consumer Event (Mon m) (Either String x)
withoutEl fo getRes fmod
    = getRes fo >>= either (return . Left) (\r -> lift (fmod r) >> return (Right r)) 

checkAttrs  :: (Monad m, Functor m) 
            => Consumer Event (Mon m) (Either String x) -> (x -> Mon m ()) -> Mon m Bool 
            -> Mon m (Maybe x)
checkAttrs getRes fmod ch = do
    attrs <- gets $ maybe [] snd . fsElem
    (ma, as) <- go attrs id
    maybe (return Nothing) (\x -> do
            modifyElem $ fmap $ second $ const as
            return $ Just x
        ) ma
    where
        go [] front = return (Nothing, front [])
        go (a:as) front = do
            modifyAttr $ const $ Just a 
            er <- checkAttr a
            maybe (go as $ front . (a:)) (\r -> return (Just r, front as)) er

        checkAttr (_,cs)= do
            ex <- lift $ CL.sourceList (map EventContent cs) $$ linearize =$ transPipe (flip evalStateT def) getRes
            either (return . const Nothing) (\x -> do
                    fmod x
                    is <- ch
                    return $ if is then Just x else Nothing
                ) ex
