module Damage where

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

mergeElemental :: Type -> Type -> Maybe Type
mergeElemental Heat Cold         = Just Blast
mergeElemental Cold Heat         = Just Blast
mergeElemental Heat Electricity  = Just Radiation
mergeElemental Electricity Heat  = Just Radiation
mergeElemental Heat Toxic        = Just Gas
mergeElemental Toxic Heat        = Just Gas
mergeElemental Cold Electricity  = Just Magnetic
mergeElemental Electricity Cold  = Just Magnetic
mergeElemental Cold Toxic        = Just Viral
mergeElemental Toxic Cold        = Just Viral
mergeElemental Electricity Toxic = Just Corrosive
mergeElemental Toxic Electricity = Just Corrosive
mergeElemental _ _               = Nothing


mergeBasicElementals :: [Damage] -> [Damage]
mergeBasicElementals [a]                        = [a]
mergeBasicElementals (Damage a b:Damage c d:xs) = case mergeElemental b d of
    Just x  -> Damage (a + c) x        : xs
    Nothing -> Damage a b : mergeBasicElementals (Damage c d : xs)

mergeElementals :: [Damage] -> [Damage]
mergeElementals d = sumByDamageType p ++ c ++ e
    where
        p = [Damage a t | Damage a t <- d, t `elem` physical]
        c = [Damage a t | Damage a t <- d, t `elem` combined]
        e = mergeBasicElementals [Damage a t | Damage a t <- d, t `elem` elemental]
