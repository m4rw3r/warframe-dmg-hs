module Utils where

import Data.List

findMaximum :: (a -> Float) -> (Float, a) -> a -> (Float, a)
findMaximum f (av, a) b = if bv > av then (bv, b) else (av, a)
    where
        bv = f b

cmpFst :: Ord a => (a, b) -> (a, b) -> Ordering
cmpFst (a, _) (b, _) = compare a b

cmpSnd :: Ord b => (a, b) -> (a, b) -> Ordering
cmpSnd (_, a) (_, b) = compare a b

-- Finds the N max values
findMaximumN :: Int -> (a -> Float) -> [(Float, a)] -> a -> [(Float, a)]
findMaximumN n f as b = take n vs
    where
        vs = sortBy (flip cmpFst) (as ++ [(f b, b)])

