{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.TotalArray.Generic where

import Prelude hiding ((!!), mapM, mapM_, (.))
import qualified Prelude as P

import Control.Monad (guard)
import Control.Monad.Primitive
import qualified Data.Vector.Generic as G
import Data.Void
import GHC.Exts (coerce)

import Data.Small

newtype GArray vec k a = Array (vec a)
  deriving (Functor, Foldable, Traversable)

--type Array = GArray V.Vector
--type UArray = GArray U.Vector

-- not sure if these are useful:
-- length, null
--
-- not trivial to implement:
-- head, last

-- Indexing

(!) :: (Small k, G.Vector vec a) => GArray vec k a -> k -> a
(!) (Array v) k = v G.! toIndex k

indexM :: (Small k, G.Vector vec a, Monad m) => GArray vec k a -> k -> m a
indexM (Array v) k = G.indexM v (toIndex k)


-- Initialization

empty :: (G.Vector vec a) => GArray vec Void a
empty = Array G.empty

singleton :: (G.Vector vec a) => a -> GArray vec () a
singleton = Array P.. G.singleton

replicate :: (Small k, G.Vector vec a) => a -> GArray vec k a
replicate a = tabulate (const a)

tabulate
  :: forall k vec a. (Small k, G.Vector vec a)
  => (k -> a)
  -> GArray vec k a
tabulate f = Array $ G.generate (numValues ([] :: [k])) $
  f P.. unsafeFromIndex

iterate
  :: forall k vec a. (Small k, G.Vector vec a)
  => (a -> a) -> a -> GArray vec k a
iterate f x = Array $ G.iterateN (numValues ([] :: [k])) f x

-- Monadic Initialization

replicateM :: (Small k, G.Vector vec a, Monad m) => m a -> m (GArray vec k a)
replicateM a = tabulateM (const a)

tabulateM
  :: forall k vec a m. (Small k, G.Vector vec a, Monad m)
  => (k -> m a)
  -> m (GArray vec k a)
tabulateM f =
  fmap Array $ G.generateM (numValues ([] :: [k])) $ f P.. unsafeFromIndex

-- TODO: Unfolding

-- TODO: Enumeration

-- Restricting memory usage

force :: (G.Vector vec a) => GArray vec k a -> GArray vec k a
force (Array v) = Array $ G.force v

-- Bulk updates

(//)
  :: (Small k, G.Vector vec a)
  => GArray vec k a -> [(k, a)] -> GArray vec k a
Array v // upds = Array $ v G.// do
  (k, a) <- upds
  return (toIndex k, a)

update
  :: (Small k, G.Vector vec a, G.Vector vec (k, a), G.Vector vec (Int, a))
  => GArray vec k a -> vec (k, a) -> GArray vec k a
update (Array v) upds = Array $ G.update v $
  G.map (\(k, a) -> (toIndex k, a)) upds

-- Accumulations

accum
  :: (Small k, G.Vector vec a)
  => (a -> b -> a) -> GArray vec k a -> [(k, b)] -> GArray vec k a
accum f (Array v) upds = Array $ G.accum f v $ do
  (k, a) <- upds
  return (toIndex k, a)

accumulate
  :: (Small k, G.Vector vec a, G.Vector vec (k, b), G.Vector vec (Int, b))
  => (a -> b -> a) -> GArray vec k a -> vec (k, b) -> GArray vec k a
accumulate f (Array v) upds = Array $ G.accumulate f v $
  G.map (\(k, a) -> (toIndex k, a)) upds

-- Permutations

reverse :: (G.Vector vec a) => GArray vec k a -> GArray vec k a
reverse (Array v) = Array $ G.reverse v

backpermute
  :: (G.Vector vec a, G.Vector vec Int)
  => vec a -> GArray vec k Int -> GArray vec k a
backpermute t (Array v) = Array $ G.backpermute t v

(.)
  :: (Small k1, G.Vector vec a, G.Vector vec Int, G.Vector vec k1)
  => GArray vec k1 a -> GArray vec k k1 -> GArray vec k a
Array t . Array v = Array $ G.backpermute t $ G.map toIndex v

-- Indexing

indexed
  :: (G.Vector vec a, G.Vector vec (Int, a))
  => GArray vec k a -> GArray vec k (Int, a)
indexed (Array v) = Array $ G.indexed v

-- Mapping

map :: (G.Vector vec a, G.Vector vec b)
  => (a -> b) -> GArray vec k a -> GArray vec k b
map f (Array v) = Array (G.map f v)

imap
  :: (Small k, G.Vector vec a, G.Vector vec b)
  => (k -> a -> b) -> GArray vec k a -> GArray vec k b
imap f (Array v) = Array (G.imap (\i a -> f (unsafeFromIndex i) a) v)

-- Monadic mapping

mapM :: (G.Vector vec a, G.Vector vec b, Monad m)
  => (a -> m b) -> GArray vec k a -> m (GArray vec k b)
mapM f (Array v) = Array <$> G.mapM f v

{-
imapM :: (Small k, G.Vector vec a, G.Vector vec b, Monad m)
    => (k -> a -> m b) -> GArray vec k a -> m (GArray vec k b)
imapM f (Array v) = Array <$> G.imapM (\i a -> f (unsafeFromIndex i) a) v
-}

mapM_ :: (G.Vector vec a, Monad m) => (a -> m b) -> GArray vec k a -> m ()
mapM_ f (Array v) = G.mapM_ f v

{-
imapM_ :: (Small k, G.Vector vec a, Monad m)
    => (k -> a -> m b) -> GArray vec k a -> m ()
imapM_ f (Array v) = G.imapM_ (\i a -> f (unsafeFromIndex i) a) v
-}

forM :: (G.Vector vec a, G.Vector vec b, Monad m)
    => GArray vec k a -> (a -> m b) -> m (GArray vec k b)
forM = flip mapM

forM_ :: (G.Vector vec a, Monad m) => GArray vec k a -> (a -> m b) -> m ()
forM_ = flip mapM_

-- zipping

zipWith
  :: (G.Vector vec a, G.Vector vec b, G.Vector vec c)
  => (a -> b -> c)
  -> GArray vec k a
  -> GArray vec k b
  -> GArray vec k c
zipWith f (Array v) (Array v1) = Array (G.zipWith f v v1) -- suboptimal

zip :: (G.Vector vec a, G.Vector vec b, G.Vector vec (a, b))
    => GArray vec k a
    -> GArray vec k b
    -> GArray vec k (a, b)
zip (Array v) (Array v1) = Array (G.zip v v1) -- suboptimal

-- monadic zipping

zipWithM
  :: (G.Vector vec a, G.Vector vec b, G.Vector vec c, Monad m)
  => (a -> b -> m c)
  -> GArray vec k a
  -> GArray vec k b
  -> m (GArray vec k c)
zipWithM f (Array v) (Array v1) = Array <$> G.zipWithM f v v1 -- suboptimal

-- unzipping

unzip
  :: (G.Vector vec a, G.Vector vec b, G.Vector vec (a, b))
  => GArray vec k (a, b) -> (GArray vec k a, GArray vec k b)
unzip (Array vec) = coerce $ G.unzip vec

-- filtering (none)

-- maybe inplace partition

-- searching

elem :: (G.Vector vec a, Eq a) => a -> GArray vec k a -> Bool
elem a (Array vec) = G.elem a vec

notElem :: (G.Vector vec a, Eq a) => a -> GArray vec k a -> Bool
notElem a (Array vec) = G.notElem a vec

find
  :: (G.Vector vec a, Small k)
  => (a -> Bool) -> GArray vec k a -> Maybe a
find f (Array vec) = G.find f vec

findIndex
  :: (G.Vector vec a, Small k) => (a -> Bool) -> GArray vec k a -> Maybe k
findIndex f (Array vec) = unsafeFromIndex <$> G.findIndex f vec

findIndicies
  :: (G.Vector vec a, G.Vector vec k, G.Vector vec Int, Small k)
  => (a -> Bool) -> GArray vec k a -> vec k
findIndicies f (Array vec) = G.map unsafeFromIndex $ G.findIndices f vec

elemIndex
  :: (G.Vector vec a, Eq a, Small k) => a -> GArray vec k a -> Maybe k
elemIndex f (Array vec) = unsafeFromIndex <$> G.elemIndex f vec

elemIndices
  :: (G.Vector vec a, G.Vector vec k, G.Vector vec Int, Eq a, Small k)
  => a -> GArray vec k a -> vec k
elemIndices f (Array vec) = G.map unsafeFromIndex $ G.elemIndices f vec

-- Folding

foldl :: (G.Vector vec b) => (a -> b -> a) -> a -> GArray vec k b -> a
foldl f x (Array vec) = G.foldl f x vec

--foldl1 :: (G.Vector vec a) => (a -> a -> a) -> GArray vec k a -> a
--foldl1 f (Array vec) = G.foldl1 f vec

foldl' :: (G.Vector vec b) => (a -> b -> a) -> a -> GArray vec k b -> a
foldl' f x (Array vec) = G.foldl' f x vec

foldr :: (G.Vector vec a) => (a -> b -> b) -> b -> GArray vec k a -> b
foldr f x (Array vec) = G.foldr f x vec

--foldr1 :: (G.Vector vec a) => (a -> a -> a) -> GArray vec k a -> a
--foldr1 f (Array vec) = G.foldr1 f vec

foldr' :: (G.Vector vec a) => (a -> b -> b) -> b -> GArray vec k a -> b
foldr' f x (Array vec) = G.foldr' f x vec

ifoldl :: (G.Vector vec b) => (a -> Int -> b -> a) -> a -> GArray vec k b -> a
ifoldl f x (Array vec) = G.ifoldl f x vec

ifoldl' :: (G.Vector vec b) => (a -> Int -> b -> a) -> a -> GArray vec k b -> a
ifoldl' f x (Array vec) = G.ifoldl' f x vec

ifoldr :: (G.Vector vec a) => (Int -> a -> b -> b) -> b -> GArray vec k a -> b
ifoldr f x (Array vec) = G.ifoldr f x vec

ifoldr' :: (G.Vector vec a) => (Int -> a -> b -> b) -> b -> GArray vec k a -> b
ifoldr' f x (Array vec) = G.ifoldr' f x vec

-- Specialised folds

all :: (G.Vector vec a) => (a -> Bool) -> GArray vec k a -> Bool
all f (Array vec) = G.all f vec

any :: (G.Vector vec a) => (a -> Bool) -> GArray vec k a -> Bool
any f (Array vec) = G.any f vec

and :: (G.Vector vec Bool) => GArray vec k Bool -> Bool
and (Array vec) = G.and vec

or :: (G.Vector vec Bool) => GArray vec k Bool -> Bool
or (Array vec) = G.or vec

sum :: (G.Vector vec a, Num a) => GArray vec k a -> a
sum (Array vec) = G.sum vec

product :: (G.Vector vec a, Num a) => GArray vec k a -> a
product (Array vec) = G.product vec

{- Could be more type safe.
maximum :: (G.Vector vec a, Ord a) => GArray vec k a -> a
maximum (Array vec) = G.maximum vec

maximumBy :: (G.Vector vec a) => (a -> a -> Ordering) -> GArray vec k a -> a
maximumBy f (Array vec) = G.maximumBy f vec

minimum :: (G.Vector vec a, Ord a) => GArray vec k a -> a
minimum (Array vec) = G.minimum vec

minimumBy :: (G.Vector vec a) => (a -> a -> Ordering) -> GArray vec k a -> a
minimumBy f (Array vec) = G.minimumBy f vec
-}

-- Monadic folds

foldM
  :: (G.Vector vec b, Monad m) => (a -> b -> m a) -> a -> GArray vec k b -> m a
foldM f x (Array vec) = G.foldM f x vec

ifoldM
  :: (G.Vector vec b, Small k, Monad m)
  => (a -> k -> b -> m a) -> a -> GArray vec k b -> m a
ifoldM f x (Array vec) = G.ifoldM (\a k b -> f a (unsafeFromIndex k) b) x vec

foldM'
  :: (G.Vector vec b, Monad m) => (a -> b -> m a) -> a -> GArray vec k b -> m a
foldM' f x (Array vec) = G.foldM' f x vec

ifoldM'
  :: (G.Vector vec b, Small k, Monad m)
  => (a -> k -> b -> m a) -> a -> GArray vec k b -> m a
ifoldM' f x (Array vec) = G.ifoldM' (\a k b -> f a (unsafeFromIndex k) b) x vec

foldM_
  :: (G.Vector vec b, Monad m) => (a -> b -> m a) -> a -> GArray vec k b -> m ()
foldM_ f x (Array vec) = G.foldM_ f x vec

ifoldM_
  :: (G.Vector vec b, Small k, Monad m)
  => (a -> k -> b -> m a) -> a -> GArray vec k b -> m ()
ifoldM_ f x (Array vec) = G.ifoldM_ (\a k b -> f a (unsafeFromIndex k) b) x vec

foldM'_
  :: (G.Vector vec b, Monad m) => (a -> b -> m a) -> a -> GArray vec k b -> m ()
foldM'_ f x (Array vec) = G.foldM'_ f x vec

ifoldM'_
  :: (G.Vector vec b, Small k, Monad m)
  => (a -> k -> b -> m a) -> a -> GArray vec k b -> m ()
ifoldM'_ f x (Array vec) = G.ifoldM'_ (\a k b -> f a (unsafeFromIndex k) b) x vec

-- Monadic sequencing

sequence
    :: (G.Vector vec a, G.Vector vec (m a), Monad m)
    => GArray vec k (m a) -> m (GArray vec k a)
sequence (Array vec) = Array <$> G.sequence vec

sequence_
    :: (G.Vector vec a, G.Vector vec (m a), Monad m)
    => GArray vec k (m a) -> m ()
sequence_ (Array vec) = G.sequence_ vec

-- Prefix sums (scans)

prescanl
    :: (G.Vector vec a, G.Vector vec b)
    => (a -> b -> a) -> a -> GArray vec k b -> GArray vec k a
prescanl f x (Array vec) = Array (G.prescanl f x vec)

prescanl'
    :: (G.Vector vec a, G.Vector vec b)
    => (a -> b -> a) -> a -> GArray vec k b -> GArray vec k a
prescanl' f x (Array vec) = Array (G.prescanl' f x vec)

postscanl
    :: (G.Vector vec a, G.Vector vec b)
    => (a -> b -> a) -> a -> GArray vec k b -> GArray vec k a
postscanl f x (Array vec) = Array (G.postscanl f x vec)

postscanl'
    :: (G.Vector vec a, G.Vector vec b)
    => (a -> b -> a) -> a -> GArray vec k b -> GArray vec k a
postscanl' f x (Array vec) = Array (G.postscanl' f x vec)

prescanr
    :: (G.Vector vec a, G.Vector vec b)
    => (a -> b -> b) -> b -> GArray vec k a -> GArray vec k b
prescanr f x (Array vec) = Array (G.prescanr f x vec)

prescanr'
    :: (G.Vector vec a, G.Vector vec b)
    => (a -> b -> b) -> b -> GArray vec k a -> GArray vec k b
prescanr' f x (Array vec) = Array (G.prescanr' f x vec)

postscanr
    :: (G.Vector vec a, G.Vector vec b)
    => (a -> b -> b) -> b -> GArray vec k a -> GArray vec k b
postscanr f x (Array vec) = Array (G.postscanr f x vec)

postscanr'
    :: (G.Vector vec a, G.Vector vec b)
    => (a -> b -> b) -> b -> GArray vec k a -> GArray vec k b
postscanr' f x (Array vec) = Array (G.postscanr' f x vec)

-- Conversions

-- Lists

toList :: (G.Vector vec a) => GArray vec k a -> [a]
toList (Array vec) = G.toList vec

fromList
  :: forall vec a k. (G.Vector vec a, Small k) => [a] -> Maybe (GArray vec k a)
fromList list = fromVector $ G.fromListN (1 + numValues ([] :: [k])) list

-- Vectors

toVector :: GArray vec k a -> vec a
toVector (Array vec) = vec

fromVector
  :: forall vec a k. (G.Vector vec a, Small k)
  => vec a -> Maybe (GArray vec k a)
fromVector vec = Array vec <$ guard (G.length vec == numValues ([] :: [k]))

-- Different vector types

convert
  :: (G.Vector vec a, G.Vector vec1 a) => GArray vec k a -> GArray vec1 k a
convert (Array vec) = Array $ G.convert vec

-- Mutable total arrays

freeze
  :: (G.Vector vec a, PrimMonad m)
  => GArray (G.Mutable vec (PrimState m)) k a -> m (GArray vec k a)
freeze (Array vec) = Array <$> G.freeze vec

thaw
  :: (G.Vector vec a, PrimMonad m)
  => GArray vec k a -> m (GArray (G.Mutable vec (PrimState m)) k a)
thaw (Array vec) = Array <$> G.thaw vec

copy
  :: (G.Vector vec a, PrimMonad m)
  => GArray (G.Mutable vec (PrimState m)) k a -> GArray vec k a -> m ()
copy (Array mvec) (Array vec) = G.copy mvec vec

unsafeFreeze
  :: (G.Vector vec a, PrimMonad m)
  => GArray (G.Mutable vec (PrimState m)) k a -> m (GArray vec k a)
unsafeFreeze (Array vec) = Array <$> G.unsafeFreeze vec

unsafeThaw
  :: (G.Vector vec a, PrimMonad m)
  => GArray vec k a -> m (GArray (G.Mutable vec (PrimState m)) k a)
unsafeThaw (Array vec) = Array <$> G.unsafeThaw vec
