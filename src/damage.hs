module Damage (Damage (..), Type (..), physical, elemental, combined, sumByDamageType, mergeType, mergeElementals, sumDamage) where

import Data.List

data Type = Impact | Puncture | Slash | Heat | Cold | Electricity | Toxic | Blast | Corrosive | Gas | Magnetic | Radiation | Viral
    deriving (Eq, Ord, Enum, Read, Show, Bounded)

data Damage = Damage Float Type
    deriving (Show, Eq)

physical  :: [Type]
elemental :: [Type]
combined  :: [Type]
physical  = [Impact, Puncture, Slash]
elemental = [Heat, Cold, Electricity, Toxic]
combined  = [Blast, Corrosive, Gas, Magnetic, Radiation, Viral]

sumByDamageType :: [Damage] -> [Damage]
sumByDamageType d = [Damage (sum [a | Damage a b <- d, b == t]) t | t <- types]
    where
        types = nub [t | Damage _ t <- d]

-- | mergeType merges the given basic elemental 'Type' into complex types.
-- Gives Nothing in case the given types cannot be merged.
mergeType :: Type -> Type -> Maybe Type
mergeType Heat Cold         = Just Blast
mergeType Cold Heat         = Just Blast
mergeType Heat Electricity  = Just Radiation
mergeType Electricity Heat  = Just Radiation
mergeType Heat Toxic        = Just Gas
mergeType Toxic Heat        = Just Gas
mergeType Cold Electricity  = Just Magnetic
mergeType Electricity Cold  = Just Magnetic
mergeType Cold Toxic        = Just Viral
mergeType Toxic Cold        = Just Viral
mergeType Electricity Toxic = Just Corrosive
mergeType Toxic Electricity = Just Corrosive
mergeType _ _               = Nothing

mergeBasicElementals :: [Damage] -> [Damage]
mergeBasicElementals []                         = []
mergeBasicElementals [a]                        = [a]
mergeBasicElementals (Damage a b:Damage c d:xs) = case mergeType b d of
    Just x  -> Damage (a + c) x : mergeBasicElementals xs
    Nothing -> Damage a b       : mergeBasicElementals (Damage c d : xs)

mergeElementals :: [Damage] -> [Damage]
mergeElementals d = sumByDamageType p ++ c ++ e
    where
        p = [Damage a t | Damage a t <- d, t `elem` physical]
        c = [Damage a t | Damage a t <- d, t `elem` combined]
        e = mergeBasicElementals [Damage a t | Damage a t <- d, t `elem` elemental]

sumDamage :: [Damage] -> Float
sumDamage d = sum [a | Damage a _ <- d]
