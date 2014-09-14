module Utils where

import Data.List
import Debug.Trace (trace)

lg :: Show a => a -> a
lg a = trace (show a) a

findMaximum :: (Num b, Ord b) => (a -> b) -> (b, a) -> a -> (b, a)
findMaximum f (av, a) b = if bv > av then (bv, b) else (av, a)
    where
        bv = f b

cmpFst :: Ord a => (a, b) -> (a, b) -> Ordering
cmpFst (a, _) (b, _) = compare a b

cmpSnd :: Ord b => (a, b) -> (a, b) -> Ordering
cmpSnd (_, a) (_, b) = compare a b

-- Finds the N max values
findMaximumN :: (Num b, Ord b) => Int -> (a -> b) -> [(b, a)] -> a -> [(b, a)]
findMaximumN n f as b = take n vs
    where
        vs = sortBy (flip cmpFst) (as ++ [(f b, b)])

