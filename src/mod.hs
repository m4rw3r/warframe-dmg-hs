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
    | ReloadSpeed Float
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
