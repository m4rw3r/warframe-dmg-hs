import Data.List
import Math.Combinat.Sets (choose)

import Damage (Damage (..), mergeElementals)
import Mod
import Weapon
import qualified Rifles

mods :: [Mod]
mods = forceModsByNames ["Serration", "Speed Trigger", "Split Chamber", "Heavy Caliber", "Stormbringer", "Infected Clip", "Point Strike", "Vital Sense"] Rifles.mods

combos :: [[Mod]]
combos = choose 8 Rifles.mods

-- findMaximum :: (a -> Float) -> (Float, a) -> a -> (Float, a)
-- findMaximum f (av, a) b = if bv > av then (bv, b) else (av, a)
--     where
--         bv = f b

cmpFst :: Ord a => (a, b) -> (a, b) -> Ordering
cmpFst (a, _) (b, _) = compare a b

-- Finds the N max values
findMaximumN :: Int -> (a -> Float) -> [(Float, a)] -> a -> [(Float, a)]
findMaximumN n f as b = take n vs
    where
        vs = sortBy (flip cmpFst) (as ++ [(f b, b)])

resultToLine :: (Float, [Mod]) -> String
resultToLine (v, m) = show v ++ ": " ++ intercalate ", " (map modName m)

sumAllDamage :: [Damage] -> Float
sumAllDamage d = sum [a | Damage a _ <- d]

main :: IO ()
main = print "test"
    >> print Rifles.lanka
    >> print (applyMods Rifles.vulkar mods)
    >> print l
    >> print (damagePerSecond l)
    >> print (mergeElementals (damage l))
    >> print (mergeElementals $ damage (applyMods Rifles.vulkar mods))
    >> print (applyMods Rifles.lanka (forceModsByNames ["Critical Delay","Cryo Rounds","Heavy Caliber","Hellfire","Point Strike","Serration","Split Chamber","Vital Sense"] Rifles.mods))
    >> print (damagePerSecond (applyMods Rifles.lanka (forceModsByNames ["Critical Delay","Cryo Rounds","Heavy Caliber","Hellfire","Point Strike","Serration","Split Chamber","Vital Sense"] Rifles.mods)))
    >> print (length combos)
    >> mapM_ (putStrLn . resultToLine) ms
    where
        l = applyMods Rifles.lanka mods
        ms = foldl' (findMaximumN 20 (sumAllDamage . damagePerSecond . applyMods Rifles.lanka)) [] combos
