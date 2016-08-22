{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
module Foo where

import Data.Small
import Data.Word
import GHC.Generics

foo :: Char -> Int
foo = (+1) . toIndex

bar :: Int -> Either Bool Word8
bar = unsafeFromIndex . (+1)

data MyType = A | B Word8 | C Bool | D
  deriving (Generic)

instance Small MyType
instance SmallIso MyType (GenericSmall MyType) where
  toIso = GenericSmall
  fromIso = fromGenericSmall

quux :: MyType -> Int
quux x = toIndex x

data MyType2 = E MyType | F
  deriving (Generic)

instance Small MyType2
instance SmallIso MyType2 (GenericSmall MyType2) where
  toIso = GenericSmall
  fromIso = fromGenericSmall

baz :: MyType2 -> Int
baz x = toIndex x

{-
mold :: SafeInt
mold = let
  !a = literal 3
  !b = literal 4
  in a + b
-}
