{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Small.Internal
  ( Small(..)
  , SmallIso(..)
  , numValues
  , toIndex
  , fromIndex
  , fromIndex'
  , unsafeFromIndex
  , GenericSmall(..)
  ) where

import Data.Char
import Data.Int
import Data.Maybe
import Data.MySafeInt
import Data.Proxy
import Data.Void
import Data.Word
import GHC.Exts (chr#, Int(..), Char(..))
import GHC.Generics

-- | A class of types which are small enough that their values can be
-- reaslistically enumerated in memory.
--
-- An ill-bahaved instance can defeat type safety of this package. For this
-- reason, the methods of this class are not publicly exported. The only way
-- for an user to create an instance for this class is to reduce it to another
-- existing instance. To do so, make your type an instance of the 'SmallIso'
-- class, then create an empty instance declaration for 'Small'.
--
-- A @Small a@ instance implicitly defines an ordering on @a@, observable
-- as @('compare' \``on`` 'toIndex')@. When @a@ is
-- also an @Ord@, it is recommended to make the both orders the same, to avoid
-- surprise. This can be achieved by defining 'toIso' and 'fromIso' to be
-- order-preserving functions.
class Small a where
  numValues_ :: Proxy a -> SafeInt
  toIndex_ :: a -> Int
  unsafeFromIndex_ :: Int -> a

  default numValues_ :: forall b. (SmallIso a b) => Proxy a -> SafeInt
  numValues_ = defaultNumValues
  {-# INLINE numValues_ #-}

  default toIndex_ :: forall b. (SmallIso a b) => a -> Int
  toIndex_ = toIndex_ . toIso
  {-# INLINE toIndex_ #-}

  default unsafeFromIndex_ :: forall b. (SmallIso a b) => Int -> a
  unsafeFromIndex_ = fromIso . unsafeFromIndex_
  {-# INLINE unsafeFromIndex_ #-}
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

defaultNumValues :: forall a b. (SmallIso a b) => Proxy a -> SafeInt
defaultNumValues _ = numValues_ (Proxy :: Proxy b)
{-# INLINE defaultNumValues #-}

-- | A class stating that @a@ has an isormorphic to another type @b@ that has
-- a @Small@ instance.
class (Small b) => SmallIso a b | a -> b where
  toIso :: a -> b
  fromIso :: b -> a

-- | A newtype wrapper around @a@, providing a 'Small' instance based on the
-- 'Generic' instance of @a@.
newtype GenericSmall a = GenericSmall { fromGenericSmall :: a }

instance (Generic a, GSmall (Rep a)) => Small (GenericSmall a) where
  numValues_ _ = gnumValues (Proxy :: Proxy (Rep a))
  {-# INLINE numValues_ #-}
  toIndex_ (GenericSmall x) = gtoIndex (from x)
  {-# INLINE toIndex_ #-}
  unsafeFromIndex_ = GenericSmall . to . gunsafeFromIndex
  {-# INLINE unsafeFromIndex_ #-}

-- | The number of possible values of type @a@.
numValues :: forall p a. (Small a) => p a -> Int
numValues _ = fromSafe $ numValues_ (Proxy :: Proxy a)

-- | Maps @a@ to @[0 .. n-1]@ where @n@ is the number of values in @a@.
--
-- The inverse is @fromIndex@.
toIndex :: (Small a) => a -> Int
toIndex a = toIndex_ a

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
unsafeFromIndex i = unsafeFromIndex_ i

instance Small Void where
  numValues_ _ = literal 0
  {-# INLINE numValues_ #-}
  toIndex_ = error "toIndex Void"
  {-# INLINE toIndex_ #-}
  unsafeFromIndex_ = error "unsafeFromIndex Void"
  {-# INLINE unsafeFromIndex_ #-}

instance Small () where
  numValues_ _ = literal 1
  {-# INLINE numValues_ #-}
  toIndex_ () = 0
  {-# INLINE toIndex_ #-}
  unsafeFromIndex_ !_ = ()
  {-# INLINE unsafeFromIndex_ #-}

instance Small Bool where
  numValues_ _ = literal 2
  {-# INLINE numValues_ #-}
  toIndex_ False = 0
  toIndex_ True = 1
  {-# INLINE toIndex_ #-}
  unsafeFromIndex_ 0 = False
  unsafeFromIndex_ _ = True
  {-# INLINE unsafeFromIndex_ #-}

instance Small Word8 where
  numValues_ _ = literal 256
  {-# INLINE numValues_ #-}
  toIndex_ = fromIntegral
  {-# INLINE toIndex_ #-}
  unsafeFromIndex_ = fromIntegral
  {-# INLINE unsafeFromIndex_ #-}

instance Small Int8 where
  numValues_ _ = literal 256
  {-# INLINE numValues_ #-}
  toIndex_ = (+128) . fromIntegral
  {-# INLINE toIndex_ #-}
  unsafeFromIndex_ = fromIntegral . subtract 128
  {-# INLINE unsafeFromIndex_ #-}

instance Small Word16 where
  numValues_ _ = literal 65536
  {-# INLINE numValues_ #-}
  toIndex_ = fromIntegral
  {-# INLINE toIndex_ #-}
  unsafeFromIndex_ = fromIntegral
  {-# INLINE unsafeFromIndex_ #-}

instance Small Int16 where
  numValues_ _ = literal 65536
  {-# INLINE numValues_ #-}
  toIndex_ = (+32768) . fromIntegral
  {-# INLINE toIndex_ #-}
  unsafeFromIndex_ = fromIntegral . subtract 32768
  {-# INLINE unsafeFromIndex_ #-}

instance Small Char where
  numValues_ _ = literal $ ord maxBound + 1
  {-# INLINE numValues_ #-}
  toIndex_ = ord
  {-# INLINE toIndex_ #-}
  unsafeFromIndex_ (I# i) = C# (chr# i)
  {-# INLINE unsafeFromIndex_ #-}

instance (Small a, Small b) => Small (a, b) where
  numValues_ _ = numValues_ (Proxy :: Proxy a) * numValues_ (Proxy :: Proxy b)
  {-# INLINE numValues_ #-}
  toIndex_ (x, y) = gtoIndex (K1 x :*: K1 y)
  {-# INLINE toIndex_ #-}
  unsafeFromIndex_ i = case gunsafeFromIndex i of
    K1 x :*: K1 y -> (x, y)
  {-# INLINE unsafeFromIndex_ #-}

instance (Small a, Small b, Small c) => Small (a, b, c) where
  numValues_ _
    = numValues_ (Proxy :: Proxy a)
    * numValues_ (Proxy :: Proxy b)
    * numValues_ (Proxy :: Proxy c)
  {-# INLINE numValues_ #-}
  toIndex_ (a, b, c) = toIndex_ ((a, b), c)
  {-# INLINE toIndex_ #-}
  unsafeFromIndex_ x = case unsafeFromIndex_ x of
    ((a, b), c) -> (a, b, c)
  {-# INLINE unsafeFromIndex_ #-}

instance (Small a, Small b, Small c, Small d) => Small (a, b, c, d) where
  numValues_ _
    = numValues_ (Proxy :: Proxy a)
    * numValues_ (Proxy :: Proxy b)
    * numValues_ (Proxy :: Proxy c)
    * numValues_ (Proxy :: Proxy d)
  {-# INLINE numValues_ #-}
  toIndex_ (a, b, c, d) = toIndex_ ((a, b), (c, d))
  {-# INLINE toIndex_ #-}
  unsafeFromIndex_ x = case unsafeFromIndex_ x of
    ((a, b), (c, d)) -> (a, b, c, d)
  {-# INLINE unsafeFromIndex_ #-}

instance (Small a, Small b) => Small (Either a b) where
  numValues_ _ = numValues_ (Proxy :: Proxy a) + numValues_ (Proxy :: Proxy b)
  {-# INLINE numValues_ #-}
  toIndex_ = gtoIndex . either (L1 . K1) (R1 . K1)
  {-# INLINE toIndex_ #-}
  unsafeFromIndex_ i = case gunsafeFromIndex i of
    L1 (K1 x) -> Left x
    R1 (K1 y) -> Right y
  {-# INLINE unsafeFromIndex_ #-}

-- Generics

class GSmall a where
  gnumValues :: Proxy a -> SafeInt
  gtoIndex :: a x -> Int
  gunsafeFromIndex :: Int -> a x

instance GSmall V1 where
  gnumValues _ = literal 0
  {-# INLINE gnumValues #-}
  gtoIndex = error "gtoIndex V1"
  {-# INLINE gtoIndex #-}
  gunsafeFromIndex = error "gunsafeFromIndex V1"
  {-# INLINE gunsafeFromIndex #-}

instance GSmall U1 where
  gnumValues _ = literal 1
  {-# INLINE gnumValues #-}
  gtoIndex U1 = 0
  {-# INLINE gtoIndex #-}
  gunsafeFromIndex _ = U1
  {-# INLINE gunsafeFromIndex #-}

instance (Small a) => GSmall (K1 i a) where
  gnumValues _ = numValues_ (Proxy :: Proxy a)
  {-# INLINE gnumValues #-}
  gtoIndex (K1 x) = toIndex_ x
  {-# INLINE gtoIndex #-}
  gunsafeFromIndex i = K1 (unsafeFromIndex_ i)
  {-# INLINE gunsafeFromIndex #-}

instance (GSmall x, GSmall y) => GSmall (x :*: y) where
  gnumValues _ = gnumValues (Proxy :: Proxy x) * gnumValues (Proxy :: Proxy y)
  {-# INLINE gnumValues #-}
  gtoIndex (a :*: b) = gtoIndex a * fromSafe (gnumValues (Proxy :: Proxy x)) + gtoIndex b
  {-# INLINE gtoIndex #-}
  gunsafeFromIndex n = a :*: b
    where
      !(x, y) = quotRem n (fromSafe $ gnumValues (Proxy :: Proxy x))
      !a = gunsafeFromIndex x
      !b = gunsafeFromIndex y
  {-# INLINE gunsafeFromIndex #-}

instance (GSmall x, GSmall y) => GSmall (x :+: y) where
  gnumValues _ = gnumValues (Proxy :: Proxy x) + gnumValues (Proxy :: Proxy y)
  {-# INLINE gnumValues #-}
  gtoIndex (L1 a) = gtoIndex a
  gtoIndex (R1 b) = fromSafe (gnumValues (Proxy :: Proxy x)) + gtoIndex b
  {-# INLINE gtoIndex #-}
  gunsafeFromIndex i
    | i < k = L1 $! gunsafeFromIndex i
    | otherwise = R1 $! gunsafeFromIndex (i - k)
    where
      !k = fromSafe $ gnumValues (Proxy :: Proxy x)
  {-# INLINE gunsafeFromIndex #-}

instance (GSmall x) => GSmall (M1 i c x) where
  gnumValues _ = gnumValues (Proxy :: Proxy x)
  {-# INLINE gnumValues #-}
  gtoIndex (M1 x) = gtoIndex x
  {-# INLINE gtoIndex #-}
  gunsafeFromIndex i = M1 (gunsafeFromIndex i)
  {-# INLINE gunsafeFromIndex #-}
