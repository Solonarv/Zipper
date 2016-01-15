module Zipper where

import Data.Functor
import Data.Foldable
import Data.Traversable
import Data.List
import Data.Bool
import Data.Set

import Control.Applicative
import Control.Comonad

-- | Note: the implementations for zleft and zright make this an infinite zipper.
-- | If either the left or right list is infinite, there may be trouble.
data Zipper a = Zipper [a] a [a] deriving Eq

zfromList :: [a] -> Zipper a
zfromList (x:xs) = Zipper [] x xs

zleft, zright :: Zipper a -> Zipper a

zleft z@(Zipper [] _ []) = z
zleft (Zipper [] c rs) = zleft (Zipper (reverse rs) c [])
zleft (Zipper (l:ls) c rs) = Zipper ls l (c:rs)

zright z@(Zipper [] _ []) = z
zright (Zipper ls c []) = zright (Zipper [] c (reverse ls))
zright (Zipper ls c (r:rs)) = Zipper (c:ls) r rs

zlefts, zrights :: Zipper a -> [Zipper a]
zlefts = tail . iterate zleft
zrights = tail . iterate zright

-- | Finite versions of zlefts/zrights
zlefts', zrights' :: Zipper a -> [Zipper a]
zlefts' z@(Zipper ls c rs) = take (length ls ) $ zlefts z
zrights' z@(Zipper ls c rs) = take (length rs) $ zrights z

flipZipper :: Zipper a -> Zipper a
flipZipper (Zipper ls c rs) = Zipper rs c ls

instance Show a => Show (Zipper a) where
    show (Zipper ls c rs) = "=["
                          ++ concat (intersperse ", " (fmap show (reverse ls)
                              ++ ["{{" ++ show c ++ "}}"]
                              ++ fmap show rs))
                          ++ "]="

instance Functor Zipper where
    fmap f (Zipper ls c rs) = Zipper (fmap f ls) (f c) (fmap f rs)

instance Comonad Zipper where
    extract (Zipper _ c _) = c
    duplicate z = Zipper (zlefts' z) z (zrights' z)

instance Foldable Zipper where
    foldr f x0 (Zipper ls c rs) = Data.Foldable.foldr f x0 $ reverse ls ++ [c] ++ rs

instance Traversable Zipper where
    traverse f (Zipper ls c rs) = Zipper <$> traverse f (reverse ls) <*> f c <*> traverse f rs

to1DCARule :: Set (Bool, Bool, Bool) -> Zipper Bool -> Bool
to1DCARule liveConfigs = \z -> (extract $ zleft z, extract z, extract $ zright z) `elem` liveConfigs