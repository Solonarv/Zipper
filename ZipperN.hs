{-# LANGUAGE
    RecordWildCards,
    ExplicitNamespaces,
    PolyKinds,
    DataKinds,
    ScopedTypeVariables
    #-}

module ZipperN (
    type ZipperN
    ) where

import Data.Array
import Data.List
import Data.Functor
import Data.Foldable
import Data.Traversable
import Data.Proxy

import Control.Applicative
import Control.Comonad

import GHC.TypeLits

import Vector

data ZipperN (n :: Nat) a = ZipperN {
    _pos :: !(Vector n Integer),
    _max :: !(Vector n Integer),
    _arr :: (Array (Vector n Integer) a)
    }

dimension :: KnownNat n => ZipperN n a -> Integer
dimension (z :: ZipperN n a) = natVal (Proxy :: Proxy n)

-- | Smart constructor
zipper :: KnownNat n => Vector n Integer -> [(Vector n Integer, a)] -> ZipperN n a
zipper hiBound vals = ZipperN { _pos = repeatV 0, _max = hiBound, _arr = array (repeatV 0, hiBound) vals }

posZ, sizeZ :: ZipperN n a -> Vector n Integer
posZ = _pos
sizeZ = _max

-- | Generate a zipper from its size and an expression for its elements
generateZipper :: KnownNat n => Vector n Integer -> (Vector n Integer -> a) -> ZipperN n a
generateZipper hiBound f = zipper hiBound [(ix, f ix) | ix <- range (repeatV 0, hiBound)]

-- | Set the zipper's pointer to a position
(|=>) :: (KnownNat n, Integral i) => ZipperN n a -> Vector n i -> ZipperN n a
z |=> p | p `within` z = z { _pos = fmap toInteger p }

-- | Move the zipper's pointer by a delta
(||=>) :: (KnownNat n, Integral i) => ZipperN n a -> Vector n i -> ZipperN n a
z ||=> d = z |=> ((+) <$> _pos z <*> fmap toInteger d)

-- | Same as ||=>, but wraps around instead of erroring if it goes out of bounds.
(||@>) :: (KnownNat n, Integral i) => ZipperN n a -> Vector n i -> ZipperN n a
z ||@> d = z |=> (mod <$> ((+) <$> _pos z <*> fmap toInteger d) <*> _max z)

-- | Get the zipper's value at a point
(|=?) :: (KnownNat n, Integral i) => ZipperN n a -> Vector n i -> a
z |=? p = extract $ z |=> p

-- | Get the zipper's value at an offset
(||=?) :: (KnownNat n, Integral i) => ZipperN n a -> Vector n i -> a
z ||=? p = extract $ z ||=> p

-- | Get the zipper's value at an offset (wraparound)
(||@?) :: (KnownNat n, Integral i) => ZipperN n a -> Vector n i -> a
z ||@? p = extract $ z ||@> p

-- | Check that a vector points into the zipper
within :: (KnownNat n, Integral i) => Vector n i -> ZipperN n a -> Bool
p `within` z = inRange (repeatV 0, fmap (\x -> x - 1) $ _max z) (fmap toInteger p)

-- | We do not show all the elements,
--   because in most cases there are way too many.
instance (Show a, KnownNat n) => Show (ZipperN n a) where
    show z@(ZipperN{..}) = (show $ sizeV _max)
                         ++ "-dimensional tensor zipper, sized "
                         ++ intercalate "x" (map show $ toList _max)
                         ++ " with "
                         ++ show (product $ toList _max)
                         ++ " entries total. The focus is on the element ("
                         ++ show (extract z)
                         ++ "), positioned at "
                         ++ show _pos

instance KnownNat n => Functor (ZipperN n) where
    fmap f z = z { _arr = fmap f (_arr z) }

instance KnownNat n => Foldable (ZipperN n) where
    foldr f x0 z = foldr f x0 (_arr z)

instance KnownNat n => Traversable (ZipperN n) where
    sequenceA ZipperN{..} = ZipperN _pos _max <$> sequenceA _arr

instance KnownNat n => Comonad (ZipperN n) where
    extract ZipperN{..} = _arr ! _pos
    duplicate z@(ZipperN{..}) = generateZipper _max (\ix -> ZipperN {_pos = ix, ..})