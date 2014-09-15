module Damage (Damage (..), Type (..), physical, elemental, combined, sumByDamageType, mergeType, mergeElementals, sumDamage) where

import Data.List

-- | Type are the different damage types used in the game.
data Type = Impact | Puncture | Slash | Heat | Cold | Electricity | Toxic | Blast | Corrosive | Gas | Magnetic | Radiation | Viral
    deriving (Eq, Ord, Enum, Read, Show, Bounded)

data Damage = Damage Float Type
    deriving (Show, Eq)

instance Ord Damage where
    Damage a _ <= Damage b _ = a <= b

-- | physical is the list of physical damage types.
physical :: [Type]
physical = [Impact, Puncture, Slash]
-- | elemental is the list of basic elemental damage types.
elemental :: [Type]
elemental = [Heat, Cold, Electricity, Toxic]
-- | combined is the list of combined elemental damage types.
combined :: [Type]
combined = [Blast, Corrosive, Gas, Magnetic, Radiation, Viral]

-- | sumByDamageType groups and summarizes all damages, preserving order of the first occurance
-- of the damage type.
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

-- | mergeBasicElementals merges elementals of the basic Heat, Cold, Electricity and Toxic types
-- into the combined elementals Blast, Corrosive, Gas, Magnetic, Radiation and Viral.
-- 
-- Preserves elemental order. Order of the Damage entries decides which combined elements
-- will be created.
--
-- NOTE: For every entry e in the given list it must satisfy Damage _ t <- e, t `elem` 'elemental'.
mergeBasicElementals :: [Damage] -> [Damage]
mergeBasicElementals []                         = []
mergeBasicElementals [a]                        = [a]
mergeBasicElementals (Damage a b:Damage c d:xs) = case mergeType b d of
    Just x  -> Damage (a + c) x : mergeBasicElementals xs
    Nothing -> Damage a b       : mergeBasicElementals (Damage c d : xs)

-- | mergeElementals merges the basic elementals to combined elementals in the damage list,
-- if possible, preserving order of the first occurance of the resulting elemental types.
-- Physical damage types will remain untouched and already existing combined elementals
-- will be summarized with damage of the same type. New elementals, or skipped basic
-- elementals will end up last.
mergeElementals :: [Damage] -> [Damage]
mergeElementals d = sumByDamageType p ++ c ++ e
    where
        p = [Damage a t | Damage a t <- d, t `elem` physical]
        c = [Damage a t | Damage a t <- d, t `elem` combined]
        e = mergeBasicElementals [Damage a t | Damage a t <- d, t `elem` elemental]

-- | sumDamage makes a naive sum of the damages, without respect to armor or
-- any other factors.
sumDamage :: [Damage] -> Float
sumDamage d = sum [a | Damage a _ <- d]
