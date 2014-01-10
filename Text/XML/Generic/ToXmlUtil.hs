{-# LANGUAGE RankNTypes, KindSignatures #-}
-- |
-- Module: Text.XML.Generic.ToXmlUtil
-- Copyright: 2013 Dmitry Olshansky
-- License: BSD3
--
-- Maintainer: olshanskydr@gmail.com
--
-- Parameters and some utility functions for ToXml 
module Text.XML.Generic.ToXmlUtil where

import Control.Applicative
import Data.Conduit
import Data.Default(Default(..))
import Data.Maybe -- (catMaybes, fromMaybe, isNothing, maybeToList)
import Data.String(IsString(..))
import Data.XML.Types(Event(..), Name(..), Content(..))
import GHC.Generics

-- | Parameters of generic transformation
data TOX = TOX  { toxTof :: TOF -- ^ functions to define name of attributes and elements
                , toxTo :: TO   -- ^ current state of transformation
                }
instance Default TOX where
    def = TOX def def

-- | Current state of transformation.
--   Generic transformation always started from Selector then Datatype then Sum is possible
--   then Constructor then optionally Products. 
--   Values are filled accordingly.
data TO = TO    { toSel     :: Maybe String                     -- ^ name of selector
                , toDT      :: Maybe (String, String)           -- ^ name of datatype and module
                , toCons    :: Maybe (String, (Fixity, Bool))   -- ^ constructor name, fixity and isRecord?
                , toRoot    :: Bool         -- ^ True on the begining of transformation, then False
                , toSum     :: Bool         -- ^ True if Sum type (valid when (isJust toCons))
                , toSumFst  :: Bool         -- ^ Is current constructor first in Sum type
                }
                deriving Show

-- | functions to get attribute and element name
data TOF = TOF { tofElName   :: TO -> Maybe Name    -- ^ Define element name by current state in 'TO'
               , tofAttrName :: TO -> Maybe Name    -- ^ Define attribute name by current state in 'TO'
               }

instance Default TO where
    def = TO    { toSel         = def
                , toDT          = def
                , toCons        = def
                , toRoot        = False
                , toSum         = False
                , toSumFst      = False
                }
instance Default TOF where
    def = TOF   { tofElName      = getNameE
                , tofAttrName    = getNameA
                }
        where
            getNameE t
                -- in :+:
                | p && f && isNothing mc && sEmpty  = fmap (fromString . fst) md
                | p && f && isNothing mc            = fmap fromString ms
                -- in Cons
                | isJust mc && not p && sEmpty      = fmap (fromString . fst) mc
                | isJust mc && not p && not sEmpty  = fmap fromString ms
                | isJust mc && p                    = fmap (fromString . fst) mc
                | otherwise                         = Nothing
                where   
                    (ms,md,mc,p,f) = (,,,,) <$> toSel <*> toDT <*> toCons <*> toSum <*> toSumFst $ t
                    sEmpty = ms `elem` [Nothing, Just ""]
            getNameA t
                | not sEmpty && isNothing md    = fmap fromString ms
                | otherwise                     = Nothing
                where   
                    (ms,md) = (,) <$> toSel <*> toDT $ t
                    sEmpty = ms `elem` [Nothing, Just ""]

-- | Name of element by 'TOX'
getElName :: TOX -> Maybe Name
getElName = tofElName . toxTof <*> toxTo

-- | Attribute name by 'TOX'
getAttrName :: TOX -> Maybe Name
getAttrName = tofAttrName . toxTof <*> toxTo

toxSum, toxNotRoot, toxDef  :: TOX -> TOX
-- | Modifying 'TOX' for Sum type
toxSum tox 
    | toSum $ toxTo tox = tox { toxTo = (toxTo tox) { toSumFst = False } }
    | otherwise         = tox { toxTo = (toxTo tox) { toSum = True
                                                    , toSumFst = True } }
-- | Modifying 'TOX' for not root
toxNotRoot tox = tox { toxTo = (toxTo tox) { toRoot = False } }
-- | Initialize 'TOX'
toxDef tox = tox { toxTo = def }

-- | Modifying 'TOX' in selector
toxSel :: forall s (t :: * -> (* -> *) -> * -> *) (f :: * -> *) a.
                         Selector s =>
                         TOX -> t s f a -> TOX
toxSel tox sel = tox { toxTo = def { toSel  = Just $ selName sel } }

-- | Modifying 'TOX' in constructor
toxCon :: forall c (t :: * -> (* -> *) -> * -> *) (f :: * -> *) a.
                         Constructor c =>
                         TOX -> t c f a -> TOX
toxCon tox con = tox { toxTo = (toxTo tox) { toCons = Just $ ((,) 
                                        <$> conName 
                                        <*> ((,) 
                                            <$> conFixity 
                                            <*> conIsRecord)) con } }

-- | Modifying 'TOX' in datatype
toxDT :: forall d (t :: * -> (* -> *) -> * -> *) (f :: * -> *) a.
                        Datatype d =>
                        TOX -> t d f a -> TOX
toxDT tox dt = tox { toxTo = (toxTo tox) { toDT  = Just $ ((,) <$> datatypeName <*> moduleName) dt } }

-- | Internal function to yield elements
doTo    :: Monad m 
        => TOX -> (TOX -> t -> Maybe a) -> (TOX -> t -> [(Name, [Data.XML.Types.Content])]) -> t
        -> ConduitM i Event m () -> ConduitM i Event m ()
doTo tox getAsAttr getAttrs x next
    = maybe 
        (maybe 
            next 
            (\n -> do
                yield $ EventBeginElement n $ getAttrs tox x
                next
                yield $ EventEndElement n
            ) $ getElName tox
        )
        (const $ return ()) 
        $ getAsAttr tox x

