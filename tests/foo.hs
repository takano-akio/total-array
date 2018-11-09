{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingVia #-}
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

data MyType3 = G Bool | H
  deriving (Generic)
  deriving Small via (GenericSmall MyType3)

newtype MyType4 = I Word8
  deriving Small via (IsoSmall MyType4)

instance SmallIso MyType4 Word8 where
  toIso (I x) = x + 44
  fromIso x = I $ x - 44

{-
mold :: SafeInt
mold = let
  !a = literal 3
  !b = literal 4
  in a + b
-}
