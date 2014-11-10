import Data.List
import Math.Combinat.Sets (choose)
import qualified Numeric.Probability.Distribution as Dist
import Text.Printf

import Armor
import Damage
import Mod
import Weapon
import Utils
import Mods
import Weapons.Rifles
import Weapons.Pistols
import Weapons.Shotguns

grineerLancer :: Enemy
grineerLancer = Enemy [ArmorValue ClonedFlesh 0, ArmorValue Ferrite 100] 1

-- |modResultToLine displays a single line of a mod result, damage first, then the list of mods.
modResultToLine :: (Float, [Mod]) -> String
modResultToLine (v, m) = show v ++ ": " ++ intercalate ", " (map modName m)

-- |shotProbabilities dispolays the probabilities of different damages occuring for a particular weapon.
shotProbabilitiesFor :: Weapon -> String
shotProbabilitiesFor w = Dist.pretty (printf "%2.2f%%") $ damageProbabilities w

-- |dps calculates the total sum of the weapon's damage per second, not taking armor into account.
dps :: Weapon -> Float
dps = sumDamage . damagePerSecond

-- |dpsVsEnemyAtLevel calculates the total sum of the weapon's damage per second, adjusted for armor and health-types of the given enemy e at level l.
dpsVsEnemyAtLevel :: Enemy -> Int -> Weapon -> Float
dpsVsEnemyAtLevel e l = sumDamage . damageOnEnemy e l . damagePerSecond

-- |mostCommonDamage gives the most probable damage occurrence for a particular weapon.
mostCommonDamage :: Weapon -> Float
mostCommonDamage = sumDamage . snd . mostCommonDamagePerShot

-- |bestNModsFor gives a list of the best N mod-combinations for a particular rifle
-- given a rating function f.
bestNModsFor :: Int -> (Weapon -> Float) -> Weapon -> [(Float, [Mod])]
bestNModsFor n f w = foldl' (findMaximumN n (f . applyMods w)) [] (choose 8 $ modsFor w)

main :: IO ()
main = mapM_ (putStrLn . modResultToLine) (bestNModsFor 20 (dpsVsEnemyAtLevel grineerLancer 200) lanka)
