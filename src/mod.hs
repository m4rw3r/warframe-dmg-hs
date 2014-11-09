module Mod where

import Data.List (find)
import Text.Printf
import qualified Damage (Type)
import WeaponType

data ModRequirement =
    ModType WeaponType
    | WeaponName String
        deriving (Show, Eq)

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
    | ReloadSpeed Float
    | Status Float
        deriving (Eq)

instance Show ModValue where
    show (Accuracy a)          = printf "%+.0f%% Accuracy" (a * 100)
    show (AnyDamage a)         = printf "%+.0f%% Damage" (a * 100)
    show (Capacity a)          = printf "%+.0f%% Ammo capacity" (a * 100)
    show (CritChance a)        = printf "%+.0f%% Critical chance" (a * 100)
    show (CritMultiplier a)    = printf "%+.0f%% Critical multiplier" (a * 100)
    show (ElementalDamage a t) = printf "%+.0f%% Extra %s damage" (a * 100) (show t)
    show (FireRate a)          = printf "%+.0f%% Fire rate" (a * 100)
    show (MagazineCapacity a)  = printf "%+.0f%% Magazine capacity" (a * 100)
    show (Multishot a)         = printf "%+.0f%% Multishot" (a * 100)
    show (ReloadSpeed a)       = printf "%+.0f%% Reload speed" (a * 100)
    show (Status a)            = printf "%+.0f%% Status chance" (a * 100)

data Mod = Mod String [ModRequirement] [ModValue]
    deriving (Show)

instance Eq Mod where
    Mod _ r v == Mod _ r2 v2 = r == r2 && v == v2

instance Ord Mod where
    Mod a _ _ <= Mod b _ _ = a <= b

modName :: Mod -> String
modName (Mod n _ _) = n

modValues :: Mod -> [ModValue]
modValues (Mod _ _ v) = v

modByName :: String -> [Mod] -> Maybe Mod
modByName n = find (\x -> modName x == n)

-- | modsByNames will find the matching Mod instances of the given list of strings.
-- Gives the name of the mod in case the mod cannot be found.
-- 
-- The resulting list of Mods will be ordered in the same order as the list of
-- mod names.
modsByNames :: [String] -> [Mod] -> Either String [Mod]
modsByNames [] _     = Right []
modsByNames (x:xs) m = case (find (\y -> modName y == x) m, modsByNames xs m) of
    (Just z, Right zs) -> Right (z : zs)
    (Just _, Left zs)  -> Left zs
    (Nothing, _)       -> Left x

-- | forceModsByNames is modsByNames but will raise an error in case any mod in
-- the given list cannot be found.
forceModsByNames :: [String] -> [Mod] -> [Mod]
forceModsByNames n m = case modsByNames n m of
    Right x -> x
    Left  x -> error $ "Mod not found: " ++ x
