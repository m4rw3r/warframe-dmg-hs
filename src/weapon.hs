module Weapon (
    Weapon (..),
    applyMods,
    effectiveFireRate,
    damagePerSecond,
    damageProbabilities,
    mostCommonDamagePerShot,
    shotProbabilities,
    Shot (..)
    ) where

import Data.List (sort, maximumBy)
import Data.Tuple (swap)
import Control.Monad (liftM2)
import qualified Numeric.Probability.Distribution as Dist

import Damage
import Mod
import Utils

data Weapon =
    Weapon {
        accuracy       :: Float,
        capacity       :: Int,
        critChance     :: Float,
        critMultiplier :: Float,
        damage         :: [Damage],
        fireRate       :: Float,
        magazine       :: Int,
        multishot      :: Float,
        name           :: String,
        reload         :: Float,
        status         :: Float
    }
    deriving (Show)

sum1 :: Num a => [a] -> a
sum1 = foldr (+) 1

-- | applyDamage applies all damage-affecting values of the given ModValues, adding
-- the new elementals before the base damage of the weapon.
-- 
-- Note that elementals are concatenated before base damage, this is because
-- weapon base damage elementals are applied last when merging basic elemental
-- types into combined elementals.
applyDamage :: Weapon -> [ModValue] -> Weapon
applyDamage w m = w { damage = mergeElementals $ sumByDamageType $ concat [p, e, b] }
    where
        -- Base damage modifying constant
        k = sum1 [a | AnyDamage a <- m]
        -- New base damage of the weapon
        b = [Damage (a * k) d | Damage a d <- damage w]
        -- Total damage for scaling elemental mods
        s = sum [a | Damage a _ <- b]
        -- Elemental damage
        e = [Damage (a * s) t | ElementalDamage a t <- m,
                                t `elem` elemental]
        -- Only apply the physical damage enhancing mods to the base value of that
        -- damage type. They do not scale with the total damage of the weapon.
        -- They are also applied simultaneously as the elemental damages, causing
        -- them to not affect elemental damage like toxic or heat.
        p = [Damage (a * c) t | ElementalDamage a t <- m,
                                Damage c t'         <- b,
                                t `elem` physical,
                                t == t']

-- Why is this so ugly?
applyAccuracy       :: Weapon -> [ModValue] -> Weapon
applyCapacity       :: Weapon -> [ModValue] -> Weapon
applyCritChance     :: Weapon -> [ModValue] -> Weapon
applyCritMultiplier :: Weapon -> [ModValue] -> Weapon
applyFireRate       :: Weapon -> [ModValue] -> Weapon
applyMagazine       :: Weapon -> [ModValue] -> Weapon
applyMultishot      :: Weapon -> [ModValue] -> Weapon
applyReload         :: Weapon -> [ModValue] -> Weapon
applyStatus         :: Weapon -> [ModValue] -> Weapon
applyAccuracy w m       = w { accuracy       = sum1 [x | Accuracy x <- m] * accuracy w }
applyCapacity w m       = w { capacity       = floor $ sum1 [x | Capacity x <- m] * fromIntegral (capacity w) }
applyCritChance w m     = w { critChance     = sum1 [x | CritChance x <- m] * critChance w }
applyCritMultiplier w m = w { critMultiplier = sum1 [x | CritMultiplier x <- m] * critMultiplier w }
applyFireRate w m       = w { fireRate       = sum1 [x | FireRate x <- m] * fireRate w }
applyMagazine w m       = w { magazine       = floor $ sum1 [x | MagazineCapacity x <- m] * fromIntegral (magazine w) }
applyMultishot w m      = w { multishot      = sum [x | Multishot x <- m] + multishot w }
applyReload w m         = w { reload         = reload w / sum1 [x | ReloadSpeed x <- m] }
applyStatus w m         = w { status         = sum1 [x | Status x <- m] * status w }

applyMods :: Weapon -> [Mod] -> Weapon
applyMods w m = foldl (\x y -> y x v) w fns
    where
        v   = concatMap modValues m
        fns = [applyDamage, applyAccuracy, applyCapacity, applyCritChance, applyCritMultiplier, applyFireRate, applyMagazine, applyMultishot, applyReload, applyStatus]

-- | averageShotDamage is the average damage compensating for crit chance, critical
-- multiplier, multishot and status chance.
averageShotDamage :: Weapon -> [Damage]
averageShotDamage w = [Damage (a * m * c * s) t | Damage a t <- damage w]
    where
        m = 1 + multishot w
        c = 1 + critChance w * critMultiplier w
        -- TODO: Incorrect, currently just adding an incorrect status damage (2x total)
        s = 1 + status w

-- | effectiveFireRate takes reload and magazine size into account.
effectiveFireRate :: Weapon -> Float
effectiveFireRate w = m * f / (m + r * f)
    -- shots * (shots / second) / ( shots + seconds * (shots / second))
    where
        m = fromIntegral $ magazine w  -- shots
        f = fireRate w                 -- shots/second
        r = reload w                   -- seconds

-- | damagePerSecond is the average damage the weapon will produce during
-- a second.
damagePerSecond :: Weapon -> [Damage]
damagePerSecond w = [Damage (a * f) t | Damage a t <- averageShotDamage w]
    where
        f = effectiveFireRate w

-- | ShotEvent are the types representing a critical hit or a status proc
-- on a shot.
data ShotEvent = Critical | StatusProc
    deriving (Eq, Show, Ord, Enum)

-- | Shot is the occurence of a shot fired from the weapon. The nested list
-- is the occurences of criticals and status procs on the shot.
data Shot = Shot [ShotEvent] | NoShot
    deriving (Show, Ord)

-- | Shot also implements Eq in a way which does not differentiate between
-- the orders of the nested ShotEvents.
instance Eq Shot where
    Shot a == Shot b = and [e `elem` b | e <- a] && and [e `elem` a | e <- b]
    NoShot == NoShot = True
    _ == _ = False

-- | eventP will return a distribution of the possible occurances of e
-- given the probability p, where p can be greater than 1.
-- 
-- A probability greater than 1 means that the event will always happen
-- at least once, and that it can happen more than one time simultaneously.
eventP :: ShotEvent -> Float -> Dist.T Float [ShotEvent]
eventP e p | p < 1 = Dist.choose p [e] []
eventP e p         = liftM2 (++) (Dist.certainly [e]) (eventP e (p - 1))

-- | shotDists is the distribution of crits and status procs on a single shot.
shotDists :: Weapon -> Dist.T Float Shot
shotDists w = Dist.map Shot (liftM2 (++) (eventP Critical (critChance w)) (eventP StatusProc (status w)))

-- | multishotP is the probability of different numbers of shots fired from
-- the weapon with one pull of the trigger, as well as the probability
-- of different effects on the shots themselves.
multishotP :: Weapon -> Float -> Dist.T Float [Shot]
multishotP w p | p < 1 = Dist.map (: []) (Dist.unfold (Dist.choose p (shotDists w) (Dist.certainly NoShot)))
multishotP w p = liftM2 (++) (Dist.map (: []) (shotDists w)) (multishotP w (p - 1))

-- | shotProbabilities is the possible outcomes of firing the weapon with a
-- single pull of the trigger, in terms of number of shots, if they crit and/or
-- produce a status effect.
shotProbabilities :: Weapon -> Dist.T Float [Shot]
shotProbabilities w = Dist.lift Dist.sortP p
    where
        -- Sort the shots and then normalize, will merge occurences like
        -- (a, b) with (b, a); order does not matter
        p = Dist.norm (Dist.map sort m)
        m = multishotP w (1 + multishot w)

damageForShot :: Weapon -> Shot -> [Damage]
damageForShot w (Shot a) = [Damage (d * m * s) t | Damage d t <- damage w]
    where
        c = sum [1 | Critical <- a]
        -- becomes critMultiplier in the common case and
        -- (2 * critMultiplier - 1) for red-crits (ie. 100% < critChance < 200%, 2x Critical)
        m = 1 + c * (critMultiplier w - 1)
        -- TODO: This is wrong, it is not straight up 2x damage on a single Damage,
        -- works as an approximation for few damage types however
        s = 1 + sum [1 | StatusProc <- a] / fromIntegral (length (damage w))
damageForShot w _ = damage w

damageForShots :: Weapon -> [Shot] -> [Damage]
damageForShots w s = sumByDamageType $ concatMap (damageForShot w) s

-- | damageProbabilities are the different damages and their probabilities
-- resulting from a single pull of the trigger of the weapon.
damageProbabilities :: Weapon -> Dist.T Float [Damage]
damageProbabilities w = Dist.map (damageForShots w) (shotProbabilities w)

mostCommonDamagePerShot :: Weapon -> (Float, [Damage])
mostCommonDamagePerShot w = swap $ maximumBy cmpSnd (Dist.decons (damageProbabilities w))

