{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.MySafeInt
  ( SafeInt(..)
  , toSafe
  , fromSafe
  , literal
  ) where

import GHC.Exts
import GHC.Real (overflowError)

newtype SafeInt = SI Int
  deriving (Eq, Ord, Show)

toSafe :: Int -> SafeInt
toSafe = SI

fromSafe :: SafeInt -> Int
fromSafe (SI x) = x

literal :: Int -> SafeInt
literal = SI
{-# NOINLINE CONLIKE [1] literal #-}

instance Num SafeInt where
  (+) = addSafeInt
  (-) = subSafeInt
  (*) = mulSafeInt
  abs (SI x) = SI (abs x)
  signum (SI x) = SI (signum x)
  fromInteger = SI . fromInteger

addSafeInt :: SafeInt -> SafeInt -> SafeInt
addSafeInt (SI (I# x)) (SI (I# y)) = case addIntC# x y of
  (# r, 0# #) -> SI (I# r)
  _ -> overflowError
{-# NOINLINE [1] addSafeInt #-}

subSafeInt :: SafeInt -> SafeInt -> SafeInt
subSafeInt (SI (I# x)) (SI (I# y)) = case subIntC# x y of
  (# r, 0# #) -> SI (I# r)
  _ -> overflowError
{-# NOINLINE [1] subSafeInt #-}

mulSafeInt :: SafeInt -> SafeInt -> SafeInt
mulSafeInt (SI (I# x)) (SI (I# y)) = case mulIntMayOflo# x y of
  0# -> SI (I# (x *# y))
  _ -> overflowError
{-# NOINLINE [1] mulSafeInt #-}

literalBinOp :: (Integer -> Integer -> Integer) -> Int -> Int -> Int
literalBinOp f x y
  | z < fromIntegral (minBound :: Int) = overflowError
  | fromIntegral (maxBound :: Int) < z = overflowError
  | otherwise = fromIntegral z
  where
    z = f (fromIntegral x) (fromIntegral y)
{-# INLINE literalBinOp #-}

{-# RULES
"addSafeInt/literal"
  forall x y. addSafeInt (literal x) (literal y)
    = literal (literalBinOp (+) x y)

"subSafeInt/literal"
  forall x y. subSafeInt (literal x) (literal y)
    = literal (literalBinOp (-) x y)

"mulSafeInt/literal"
  forall x y. mulSafeInt (literal x) (literal y)
    = literal (literalBinOp (*) x y)
  #-}
