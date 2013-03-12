{-# LANGUAGE ExistentialQuantification, OverloadedStrings, DeriveGeneric, OverlappingInstances #-}
--  ScopedTypeVariables, GADTs #-}
module Main where

import Text.XML.Generic.ToXml
import Text.XML.Generic.FromXml

import Control.Monad
import Data.Conduit
import Data.Maybe
import qualified Data.Conduit.List as CL
import Data.List(foldl')
import qualified Data.Text as T
import GHC.Generics
import Text.Printf(printf)
import qualified Text.XML.Stream.Render as XR
import Data.Default(Default(..))
import Data.Monoid

main = tests xs

data ForXml = forall a. (FromXml a, ToXml a, Eq a) => X { unX :: a }

data T0 = T0
    deriving (Eq, Show, Generic)
instance ToXml T0
instance FromXml T0

data T1 = T1 T.Text
    deriving (Eq, Show, Generic)
instance ToXml T1
instance FromXml T1

data T2 = T2 {val :: Int}
    deriving (Eq, Show, Generic)
instance ToXml T2
instance FromXml T2

data T3 = T31 | T32 | T33
    deriving (Eq, Show, Generic)
instance ToXml T3
instance FromXml T3
    
data T4 = T4 {v4 :: Int, n4 :: Maybe T4}
-- data T4=T4 {n4 :: Maybe T4}
    deriving (Eq, Show, Generic)
instance ToXml T4
instance FromXml T4

toT4 k = foldl' (\t n -> T4 n $ Just t) (T4 (-1) Nothing) [0..k]

data T5 = T51 {v5 :: Int, t3 :: T3 } | T52 { v521 :: T.Text, v5 :: Int, t3 :: T3 }
    deriving (Eq, Show, Generic)
instance ToXml T5
instance FromXml T5


xs :: [(ForXml, T.Text)]
-- xs  = [(X $ T4 5 $ Just $ T4 6 Nothing, "<T4 v4=\"5\"><n4 v4=\"6\"/></T4>")]
xs  =   [ (X $ T0, "<T0/>")
        , (X $ T1 "", "<T1/>")
        , (X $ T2 5, "<T2 val=\"5\"/>")
        , (X $ T32, "<T3><T32/></T3>")
        , (X $ T4 5 $ Just $ T4 6 Nothing, "<T4 v4=\"5\"><n4 v4=\"6\"/></T4>")
        , (X $ T51 5 T32, "<T5><T51 v5=\"5\"><t3><T32/></t3></T51></T5>")
        , (X $ T52 "xxx" 5 T31, "<T5><T52 v521=\"xxx\" v5=\"5\"><t3><T31/></t3></T52></T5>")
        ]

tests xs = zipWithM (\n x -> test x >>= putStrLn . printf "Тест %i: %s" n . T.unpack) [(0::Int)..] xs

test :: (ForXml, T.Text) -> IO T.Text
test (X a, t) = fmap T.unwords $ sequence
        [ return ""
        , fmap (("\t1:" <>) . tst . (==Just t)) $ runToXml a
        , fmap (("\t2:" <>) . tst . (==Right a)) 
                $ runToXml a >>= maybe (return $ Left "err1") runFromXml
        , fmap (("\t3:" <>) . tst . (==Right a)) $ runFromXml t
        ]
    where
        tst True = " (+)"
        tst False = " (-)" 


