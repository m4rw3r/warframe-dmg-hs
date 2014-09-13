module Damage where

import Data.List

data Type = Impact | Puncture | Slash | Heat | Cold | Electricity | Toxic | Blast | Corrosive | Gas | Magnetic | Radiation | Viral
    deriving (Eq, Ord, Enum, Read, Show, Bounded)

data Damage = Damage Float Type
    deriving (Show, Eq)

physical  :: [Type]
elemental :: [Type]
physical  = [Impact, Puncture, Slash]
elemental = [Heat, Cold, Electricity, Toxic]

sumByDamageType :: [Damage] -> [Damage]
sumByDamageType d = [Damage (sum [a | Damage a b <- d, b == t]) t | t <- types]
    where
        types = nub [t | Damage _ t <- d]
