module Mod where

import Data.List (find)
import qualified Damage (Type)

data ModValue =
    Accuracy Float
    | AnyDamage Float
    | Capacity Float
    | CritChance Float
    | CritMultiplier Float
    | ElementalDamage Float Damage.Type
    | FireRate Float
    | MagazineCapacity Float
    | Multishot Float
    | Reload Float
    | Status Float
        deriving (Show, Eq)

data Mod = Mod String [ModValue]
    deriving (Show)

instance Eq Mod where
    Mod _ v == Mod _ v2 = v == v2

instance Ord Mod where
    Mod a _ <= Mod b _ = a <= b

modName :: Mod -> String
modName (Mod n _) = n

modValues :: Mod -> [ModValue]
modValues (Mod _ v) = v

modByName :: String -> [Mod] -> Maybe Mod
modByName n = find (\x -> modName x == n)

modsByNames :: [String] -> [Mod] -> [Mod]
modsByNames n = filter (\x -> modName x `elem` n)
