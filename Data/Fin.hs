{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoStarIsType #-}

-- | Small finite types.
module Data.Fin
  ( Fin
  , ReifiesSize
  , reifySize
  , reflectSize
  , unsafeFinToInt
  , unsafeIntToFin

  -- * Dictionaries
  , ReifiesSizeDict(..)
  , reifiesSizeAdd
  , reifiesSizeMul
  , unsafeForgeDict

  -- * Isomorphisms
  , factorFin
  , unfactorFin
  , splitFin
  , unsplitFin
  ) where

import Data.Proxy
import Data.Tagged
import GHC.TypeLits
import Unsafe.Coerce

import Data.MySafeInt
import Data.Small.Internal

-- | @Fin n@ represents a natural number that:
--
-- * is non-negative.
-- * is less than @n@.
-- * is representable as an @Int@.
--
-- If @n@ is an instance of 'ReifiesSize', then @Fin n@ has exactly @n@ values,
-- aside from the bottom.
newtype Fin (n :: Nat) = Fin { unFin :: Int }
  deriving (Eq, Ord)

instance ReifiesSize n => Small (Fin n) where
  numValues_ _ = toSafe $ reflectSize (Proxy :: Proxy n)
  {-# INLINE numValues_ #-}
  toIndex_ = unFin
  {-# INLINE toIndex_ #-}
  unsafeFromIndex_ = Fin
  {-# INLINE unsafeFromIndex_ #-}

reflectSizeInteger :: (ReifiesSize n) => p n -> Integer
reflectSizeInteger p = toInteger $ reflectSize p

-- | A class for natural numbers that fit in an @Int@.
class ReifiesSize (n :: Nat) where
  treflectSize :: Tagged n Int

-- | Return the number that is represented by the type @n@.
reflectSize :: forall p n. (ReifiesSize n) => p n -> Int
reflectSize _ = untag (treflectSize :: Tagged n Int)

unsafeFinToInt :: Fin n -> Int
unsafeFinToInt = unFin

unsafeIntToFin :: Int -> Fin n
unsafeIntToFin = Fin

newtype MagicSize r = MagicSize (forall n. ReifiesSize n => Proxy n -> r)

-- | Turn a given non-negative @Int@ into a type. Throw an error if a negative
-- value is given.
--
-- @
--   'reifySize' n 'reflectSize' = n
-- @
reifySize :: forall r. Int -> (forall n. ReifiesSize n => Proxy n -> r) -> r
reifySize n k
  | n < 0 = error "reifySize: negative size"
  | otherwise = unsafeCoerce (MagicSize k :: MagicSize r) n Proxy

instance ReifiesSize 0 where treflectSize = Tagged 0
instance ReifiesSize 1 where treflectSize = Tagged 1
instance ReifiesSize 2 where treflectSize = Tagged 2
instance ReifiesSize 3 where treflectSize = Tagged 3
instance ReifiesSize 4 where treflectSize = Tagged 4
instance ReifiesSize 5 where treflectSize = Tagged 5
instance ReifiesSize 6 where treflectSize = Tagged 6
instance ReifiesSize 7 where treflectSize = Tagged 7
instance ReifiesSize 8 where treflectSize = Tagged 8
instance ReifiesSize 9 where treflectSize = Tagged 9
instance ReifiesSize 10 where treflectSize = Tagged 10
instance ReifiesSize 11 where treflectSize = Tagged 11
instance ReifiesSize 12 where treflectSize = Tagged 12
instance ReifiesSize 13 where treflectSize = Tagged 13
instance ReifiesSize 14 where treflectSize = Tagged 14
instance ReifiesSize 15 where treflectSize = Tagged 15
instance ReifiesSize 16 where treflectSize = Tagged 16
instance ReifiesSize 32 where treflectSize = Tagged 32
instance ReifiesSize 64 where treflectSize = Tagged 64
instance ReifiesSize 128 where treflectSize = Tagged 128
instance ReifiesSize 256 where treflectSize = Tagged 256
instance ReifiesSize 512 where treflectSize = Tagged 512
instance ReifiesSize 1024 where treflectSize = Tagged 1024
instance ReifiesSize 2048 where treflectSize = Tagged 2048
instance ReifiesSize 65536 where treflectSize = Tagged 65536

-- | A witness that @n@ has a 'ReifiesSize' instance.
data ReifiesSizeDict (n :: Nat) where
  ReifiesSizeDict :: ReifiesSize n => ReifiesSizeDict n

-- | Try to retrieve the 'ReifiesSize' instance for @n + m@. Return a @Nothing@
-- if @n + m@ overflows @Int@.
reifiesSizeAdd
  :: forall n m
  .  ReifiesSizeDict n
  -> ReifiesSizeDict m
  -> Maybe (ReifiesSizeDict (n + m))
reifiesSizeAdd ReifiesSizeDict ReifiesSizeDict
  = unsafeForgeDict $
      reflectSizeInteger (Proxy :: Proxy n)
      + reflectSizeInteger (Proxy :: Proxy m)

-- | Try to retrieve the 'ReifiesSize' instance for @n * m@. Return a @Nothing@
-- if @n * m@ overflows @Int@.
reifiesSizeMul
  :: forall n m
  .  ReifiesSizeDict n
  -> ReifiesSizeDict m
  -> Maybe (ReifiesSizeDict (n * m))
reifiesSizeMul ReifiesSizeDict ReifiesSizeDict
  = unsafeForgeDict $
      reflectSizeInteger (Proxy :: Proxy n)
      * reflectSizeInteger (Proxy :: Proxy m)

unsafeForgeDict :: Integer -> Maybe (ReifiesSizeDict n)
unsafeForgeDict n
  | n < 0 || toInteger (maxBound :: Int) < n = Nothing
  | otherwise = reifySize (fromInteger n) $ \(_ :: Proxy n) ->
      unsafeCoerce $ Just $! (ReifiesSizeDict :: ReifiesSizeDict n)

-- | Order-preserving map from @Fin (n * m)@ to @(Fin n, Fin m)@.
factorFin :: forall n m. (ReifiesSize m) => Fin (n * m) -> (Fin n, Fin m)
factorFin (Fin x) = (Fin y, Fin z)
  where
    !(y, z) = quotRem x $ reflectSize (Proxy :: Proxy m)

-- | Order-preserving map from @(Fin n, Fin m)@ to @Fin (n * m)@.
unfactorFin :: forall n m. (ReifiesSize m) => (Fin n, Fin m) -> Fin (n * m)
unfactorFin (Fin y, Fin z) = Fin $ y * reflectSize (Proxy :: Proxy m) + z

-- | Order-preserving map from @Fin (n + m)@ to @Either (Fin n) (Fin m)@.
splitFin :: forall n m. (ReifiesSize n) => Fin (n + m) -> Either (Fin n) (Fin m)
splitFin (Fin z)
  | z < n = Left $ Fin z
  | otherwise = Right $! Fin (z - n)
  where
    !n = reflectSize (Proxy :: Proxy n)

-- | Order-preserving map from @Either (Fin n) (Fin m)@ to @Fin (n + m)@.
unsplitFin
  :: forall n m. (ReifiesSize n) => Either (Fin n) (Fin m) -> Fin (n + m)
unsplitFin = Fin . either unFin (subtract n . unFin)
  where
    !n = reflectSize (Proxy :: Proxy n)
