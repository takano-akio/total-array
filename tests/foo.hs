{-# LANGUAGE DeriveGeneric #-}
module Foo where

import Data.Small
import Data.Word
import GHC.Generics

foo :: Char -> Int
foo = (+1) . toIndex

bar :: Int -> Either Bool Word8
bar = unsafeFromIndex . (+1)

data MyType = A | B Word8 | C Bool
  deriving (Generic)

instance Small MyType

quux :: MyType -> Int
quux = toIndex
