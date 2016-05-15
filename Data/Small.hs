{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MagicHash #-}

-- | Small types.
module Data.Small
  ( Small(..)
  , numValues
  , toIndex
  , fromIndex
  , fromIndex'
  , unsafeFromIndex

  , SmallWitness
  , isomorphicSmallWitness
  ) where

import Data.Char
import Data.Int
import Data.Maybe
import Data.SafeInt
import Data.Void
import Data.Word
import GHC.Exts (chr#, Int(..), Char(..))
import GHC.Generics

-- | A class of types which are small enough that their values can be
-- reaslistically enumerated in memory.
--
-- Instead of having methods for actual operations, this class has just one
-- abstract method @smallWitness@ that contains all the operations. This is to
-- eliminate the possibility to define a malformed @Small@ instance: such an
-- instance would defeat type safety of this package.
--
-- When defining a new instance, either use the default generic implementation,
-- or take an isomorphic type that already has a @Small@ instance and use
-- 'isomorphicSmallWitness' to adapt it.
-- Such an isomorhic type always exists as @'Fin' n@ for some @n@.
--
-- A @Small a@ instance implicitly defines an ordering of @a@, observable
-- through 'toIndex' and 'fromIndex'. When @a@ is
-- also an @Ord@, it is recommended to make the both orders the same, to avoid
-- surprise. This can be achieved by passing order-preserving functions
-- to 'isomorphicSmallWitness'.
class Small a where
  -- | A witness that @a@ is indeed a small type.
  smallWitness :: SmallWitness a

  default smallWitness :: (Generic a, GSmall (Rep a)) => SmallWitness a
  smallWitness = isomorphicSmallWitness to from gsmallWitness

-- | The number of possible values of type @a@.
numValues :: forall p a. (Small a) => p a -> Int
numValues _ = fromSafe $ numValues_ (smallWitness :: SmallWitness a)

-- | Maps @a@ to @[0 .. n-1]@ where @n@ is the number of values in @a@.
--
-- The inverse is @fromIndex@.
toIndex :: (Small a) => a -> Int
toIndex a = toIndex_ smallWitness a

-- | Maps @[0 .. n-1]@ to @a@ where @n@ is the number of values in @a@. Returns
-- @Nothing@ if the argument is out of range.
--
-- The inverse is @toIndex@.
fromIndex :: forall a. (Small a) => Int -> Maybe a
fromIndex i
  | 0 <= i && i < numValues ([] :: [a]) = Just $! unsafeFromIndex i
  | otherwise = Nothing

-- | Like 'fromIndex', but thorws an exception when the argument is out of
-- range.
fromIndex' :: (Small a) => Int -> a
fromIndex' = fromMaybe (error "fromIndex': out of bounds") . fromIndex

-- | Like 'fromIndex', but causes an undefined behavior if the argument is out
-- of range.
unsafeFromIndex :: (Small a) => Int -> a
unsafeFromIndex i = unsafeFromIndex_ smallWitness i

-- | A witness that @a@ has a valid @Small@ implementation.
data SmallWitness a = SmallWitness
  { numValues_ :: !SafeInt
  , toIndex_ :: a -> Int
  , unsafeFromIndex_ :: Int -> a
  }
-- Regular laws (implementations should satisfy these, but violations don't
--  lead to an undefined behavior):
--  for all @(x :: a)@, @(p :: p a)@, @i :: Int@,
--
-- * @0 <= numValues p@
-- * @0 <= toIndex x@
-- * @toIndex x < numValues p@
-- * @unsafeFromIndex (toIndex x) = x@
-- * If @0 <= i && i < numValues p@, then @toIndex (unsafeFromIndex i) = i@.
--
-- @unsafeFromIndex@ MAY trigger an undefined behavior as long as the above
-- laws are satisfied.
--
-- Minimal laws (implementations must satisfy these, otherwise the behavior
-- is undefined):
--  for all @(x :: a)@ and @(p :: p a)@,
-- 
-- * @0 <= toIndex x@ is either @True@ or bottom.
-- * @toIndex x < numValues p@ is either @True@ or bottom.

-- | Construct a @SmallWitness@ from an existing one for an isomorphic type.
-- The given functions should be an isomorphism.
isomorphicSmallWitness
  :: (a -> b) -> (b -> a) -> SmallWitness a -> SmallWitness b
isomorphicSmallWitness forward back sw = SmallWitness
  { numValues_ = numValues_ sw
  , toIndex_ = toIndex_ sw . back
  , unsafeFromIndex_ = forward . unsafeFromIndex_ sw
  }
{-# INLINE isomorphicSmallWitness #-}

instance Small Void where
  smallWitness = SmallWitness
    { numValues_ = toSafe 0
    , toIndex_ = error "toIndex Void"
    , unsafeFromIndex_ = error "unsafeFromIndex Void"
    }
  {-# INLINE smallWitness #-}

instance Small () where
  smallWitness = SmallWitness
    { numValues_ = toSafe 1
    , toIndex_ = \() -> 0
    , unsafeFromIndex_ = \ !_ -> ()
    }
  {-# INLINE smallWitness #-}

instance Small Bool where
  smallWitness = isomorphicSmallWitness
    (either (const False) (const True))
    (\case False -> Left (); True -> Right())
    smallWitness
  {-# INLINE smallWitness #-}

instance Small Word8 where
  smallWitness = SmallWitness
    { numValues_ = toSafe 256
    , toIndex_ = fromIntegral
    , unsafeFromIndex_ = fromIntegral
    }
  {-# INLINE smallWitness #-}

instance Small Int8 where
  smallWitness = isomorphicSmallWitness
    (fromIntegral . subtract 128) ((+128) . fromIntegral)
    (smallWitness :: SmallWitness Word8)
  {-# INLINE smallWitness #-}

instance Small Word16 where
  smallWitness = SmallWitness
    { numValues_ = toSafe 65536
    , toIndex_ = fromIntegral
    , unsafeFromIndex_ = fromIntegral
    }
  {-# INLINE smallWitness #-}

instance Small Int16 where
  smallWitness = isomorphicSmallWitness
    (fromIntegral . subtract 32768) ((+32768) . fromIntegral)
    (smallWitness :: SmallWitness Word16)
  {-# INLINE smallWitness #-}

instance Small Char where
  smallWitness = SmallWitness
    { numValues_ = toSafe $ ord maxBound + 1
    , toIndex_ = ord
    , unsafeFromIndex_ = \(I# i) -> C# (chr# i)
    }
  {-# INLINE smallWitness #-}

instance (Small a, Small b) => Small (a, b) where
  smallWitness = productWitness smallWitness smallWitness
  {-# INLINE smallWitness #-}

instance (Small a, Small b, Small c) => Small (a, b, c) where
  smallWitness =
    isomorphicSmallWitness
      (\(a, (b, c)) -> (a, b, c))
      (\(a, b, c) -> (a, (b, c)))
      smallWitness
  {-# INLINE smallWitness #-}

instance (Small a, Small b, Small c, Small d) => Small (a, b, c, d) where
  smallWitness =
    isomorphicSmallWitness
      (\((a, b), (c, d)) -> (a, b, c, d))
      (\(a, b, c, d) -> ((a, b), (c, d)))
      smallWitness
  {-# INLINE smallWitness #-}

instance (Small a, Small b) => Small (Either a b) where
  smallWitness = sumWitness smallWitness smallWitness
  {-# INLINE smallWitness #-}

productWitness :: SmallWitness a -> SmallWitness b -> SmallWitness (a, b)
productWitness !wa !wb = SmallWitness
  { numValues_ = numValues_ wa * numValues_ wb
  , toIndex_ = \(a, b) ->
      toIndex_ wa a * fromSafe (numValues_ wa) + toIndex_ wb b
  , unsafeFromIndex_ = \n -> let
        !(x, y) = quotRem n (fromSafe $ numValues_ wa)
        !a = unsafeFromIndex_ wa x
        !b = unsafeFromIndex_ wb y
      in (a, b)
  }
{-# INLINE productWitness #-}

sumWitness :: SmallWitness a -> SmallWitness b -> SmallWitness (Either a b)
sumWitness !wa !wb = SmallWitness
  { numValues_ = numValues_ wa + numValues_ wb
  , toIndex_ = \case
      Left a -> toIndex_ wa a
      Right b -> fromSafe (numValues_ wa) + toIndex_ wb b
  , unsafeFromIndex_ = \i -> let
        !k = fromSafe $ numValues_ wa
      in if i < k
        then Left $! unsafeFromIndex_ wa i
        else Right $! unsafeFromIndex_ wb (i - k)
  }
{-# INLINE sumWitness #-}

-- Generics

class GSmall a where
  gsmallWitness :: SmallWitness (a x)

instance GSmall V1 where
  gsmallWitness
    = isomorphicSmallWitness (\case) (\case) (smallWitness :: SmallWitness Void)
  {-# INLINE gsmallWitness #-}

instance GSmall U1 where
  gsmallWitness = isomorphicSmallWitness (\() -> U1) (\U1 -> ()) smallWitness
  {-# INLINE gsmallWitness #-}

instance (Small a) => GSmall (K1 i a) where
  gsmallWitness = isomorphicSmallWitness K1 (\(K1 x) -> x) smallWitness
  {-# INLINE gsmallWitness #-}

instance (GSmall x, GSmall y) => GSmall (x :*: y) where
  gsmallWitness =
    isomorphicSmallWitness (\(a, b) -> a :*: b) (\(a :*: b) -> (a, b))
    $ productWitness gsmallWitness gsmallWitness
  {-# INLINE gsmallWitness #-}

instance (GSmall x, GSmall y) => GSmall (x :+: y) where
  gsmallWitness =
    isomorphicSmallWitness (either L1 R1) (\case L1 x -> Left x; R1 x -> Right x)
    $ sumWitness gsmallWitness gsmallWitness
  {-# INLINE gsmallWitness #-}

instance (GSmall x) => GSmall (M1 i c x) where
  gsmallWitness = isomorphicSmallWitness M1 (\(M1 x) -> x) gsmallWitness
  {-# INLINE gsmallWitness #-}
