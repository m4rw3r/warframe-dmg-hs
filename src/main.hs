import Data.List
import Math.Combinat.Sets (choose)
import qualified Numeric.Probability.Distribution as Dist
import Text.Printf

import Damage
import Mod
import Weapon
import Utils
import qualified Rifles
import qualified Pistols
import qualified Shotguns

mods :: [Mod]
mods = forceModsByNames ["Serration", "Speed Trigger", "Split Chamber", "Heavy Caliber", "Stormbringer", "Infected Clip", "Point Strike", "Vital Sense"] Rifles.mods
mods2 :: [Mod]
mods2 = forceModsByNames ["Malignant Force", "Hammershot", "Heavy Caliber", "High Voltage", "Point Strike", "Serration", "Vital Sense", "Speed Trigger"] Rifles.mods

combos :: [[Mod]]
combos = choose 8 Rifles.mods

pcombos :: [[Mod]]
pcombos = choose 8 Pistols.mods

scombos :: [[Mod]]
scombos = choose 8 Shotguns.mods

combos7 :: [[Mod]]
combos7 = choose 6 Rifles.mods

resultToLine :: (Float, [Mod]) -> String
resultToLine (v, m) = show v ++ ": " ++ intercalate ", " (map modName m)

probabilityToLine :: (Show a, Ord a) => Dist.T Float a -> String
probabilityToLine p = Dist.pretty (printf "%2.2f%%") p

main :: IO ()
main = print "test"
    >> putStrLn (probabilityToLine $ shotProbabilities l)
    >> print Rifles.lanka
    >> print (applyMods Rifles.vulkar mods)
    >> print l
    >> print (damagePerSecond l)
    >> print (damagePerSecond $ applyMods Rifles.lanka mods2)
    >> print (mergeElementals (damage l))
    >> print (mergeElementals $ damage (applyMods Rifles.vulkar mods))
    >> print (applyMods Rifles.lanka (forceModsByNames ["Critical Delay","Cryo Rounds","Heavy Caliber","Hellfire","Point Strike","Serration","Split Chamber","Vital Sense"] Rifles.mods))
    >> print (damagePerSecond (applyMods Rifles.lanka mods2))
    >> putStrLn (probabilityToLine $ damageProbabilities l)
    >> putStrLn (probabilityToLine $ damageProbabilities (applyMods Rifles.dread mods))
    >> putStrLn "New: "
    >> putStrLn (probabilityToLine $ damageProbabilities $ applyMods Rifles.lanka mods2)
    >> print (mostCommonDamagePerShot $ applyMods Rifles.lanka mods2)
    >> print (mostCommonDamagePerShot $ applyMods Rifles.lanka mods)
    >> print (length combos)
    -- >> mapM_ (putStrLn . resultToLine) ps
    -- >> mapM_ (putStrLn . resultToLine) ss
    >> mapM_ (putStrLn . resultToLine) ms
    -- >> putStrLn "Optimizing for most common damage: "
    -- >> mapM_ (putStrLn . resultToLine) mc
    where
        l = applyMods Rifles.lanka mods
        -- ss = foldl' (findMaximumN 20 (sumDamage . damagePerSecond . applyMods Shotguns.drakgoon)) [] scombos
        -- ps = foldl' (findMaximumN 20 (sumDamage . damagePerSecond . applyMods Pistols.despair)) [] pcombos
        ms = foldl' (findMaximumN 20 (sumDamage . damagePerSecond . applyMods Rifles.lanka)) [] combos
        -- mc = foldl' (findMaximumN 20 ((\x -> sumDamage (snd x)) . mostCommonDamagePerShot . applyMods Rifles.lanka)) [] combos7
