import Data.List
import Math.Combinat.Sets (choose)

import Damage (Damage (..), mergeElementals)
import Mod
import Weapon
import Utils
import qualified Rifles

mods :: [Mod]
mods = forceModsByNames ["Serration", "Speed Trigger", "Split Chamber", "Heavy Caliber", "Stormbringer", "Infected Clip", "Point Strike", "Vital Sense"] Rifles.mods

combos :: [[Mod]]
combos = choose 8 Rifles.mods

resultToLine :: (Float, [Mod]) -> String
resultToLine (v, m) = show v ++ ": " ++ intercalate ", " (map modName m)

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
