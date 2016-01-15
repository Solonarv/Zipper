{-# LANGUAGE
    DataKinds,
    KindSignatures,
    GADTs,
    FlexibleInstances,
    ScopedTypeVariables,
    FlexibleContexts,
    UndecidableInstances,
    RoleAnnotations,
    PolyKinds,
    ExplicitNamespaces
    #-}

module Vector (
    type Vector,    -- Constructor is NOT exported. This means you can't patternmatch.
    vector,         -- Use this smart constructor instead, it enforces necessary invariats.
    sizeV,
    zipV,
    repeatV,
    type KnownNat   -- Reexport, because it tends to be necessary in the context of anything that uses Vector
    ) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import Data.List ((++), genericReplicate, genericLength, elemIndex, genericIndex, genericTake)
import Data.Array
import Data.Function (($))
import Data.Functor
import Data.Foldable
import Data.Traversable

import Control.Applicative
import Control.Monad

import GHC.TypeLits

type role Vector nominal representational
data Vector (n :: Nat) a = Vector [a]

vector :: forall n a. KnownNat n => [a] -> Vector n a
vector xs = let len = natVal (Proxy :: Proxy n)
            in if len == genericLength xs
               then Vector xs
               else error "Vector must be initialized with the correct number of elements!"

-- | A vector's size. Useful so you don't have to do proxy-mangling in *your* code.
sizeV :: KnownNat n => Vector n a -> Integer
sizeV (v :: Vector n a) = natVal (Proxy :: Proxy n)

-- | Zip, but only for same-length vectors.
zipV :: KnownNat n => Vector n a -> Vector n b -> Vector n (a, b)
zipV = zipWithV (,)

-- | ZipWith, duh
zipWithV :: KnownNat n => (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
zipWithV f (Vector xs) (Vector ys) = Vector $ getZipList $ f <$> ZipList xs <*> ZipList ys


-- | Repeat a value n times.
repeatV :: forall n a. KnownNat n => a -> Vector n a
repeatV = Vector . genericTake (natVal (Proxy :: Proxy n)) . pure

(!@) :: (KnownNat n, Integral i) => Vector n a -> i -> a
v@(Vector vec) !@ i | 0 <= i' && i' <= sizeV v
                    = genericIndex vec i'
     where i' = toInteger i

instance (KnownNat n, Show a) => Show (Vector n a) where
    show v = show (sizeV v) ++ show (toList v)

instance (KnownNat n, Read a) => Read (Vector n a) where
    readsPrec i s = do (lm, s') <- readsPrec i s
                       guard $ lm == natVal (Proxy :: Proxy n)
                       (vals, s'') <- readsPrec i s'
                       guard $ genericLength vals == lm
                       return $ (vector vals, s'')

instance Functor (Vector n) where
    fmap f (Vector l) = Vector (fmap f l)

instance Foldable (Vector n) where
    foldr f x0 (Vector l) = foldr f x0 l

instance Traversable (Vector n) where
    sequenceA (Vector l) = fmap Vector (sequenceA l)

-- Note: this is the ZipList-like instance, which means there's no monad. This is as far as we can go.
instance KnownNat n => Applicative (Vector n) where
    pure = repeatV
    (<*>) = zipWithV ($)

instance (KnownNat n, Eq a) => Eq (Vector n a) where
    v1 == v2 = and $ zipWithV (==) v1 v2
instance (KnownNat n, Ord a) => Ord (Vector n a) where
    (Vector a1) `compare` (Vector a2) = a1 `compare` a2
instance (KnownNat n, Bounded a) => Bounded (Vector n a) where
    minBound = repeatV minBound
    maxBound = repeatV maxBound
instance (KnownNat n, Ix a) => Ix (Vector n a) where
    range (vec1, vec2) = let xs = toList vec1
                             ys = toList vec2
                             ranges = zipWith (curry range) xs ys 
                         in fmap vector $ go ranges
                         where go [] = [[]]
                               go (l:ls) = do x <- l
                                              t <- go ls
                                              [x : t]
    index bounds e = case elemIndex e (range bounds) of Just i -> i
    inRange (lo, hi) e = lo <= e && e <= hi