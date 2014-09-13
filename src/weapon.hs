module Weapon (Weapon (..), applyMods, critDamage, effectiveFireRate, damagePerSecond, averageStatusDamage) where

import Damage
import Mod

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

applyDamage :: Weapon -> [ModValue] -> Weapon
applyDamage w m = w { damage = sumByDamageType $ concat [b, e, p] }
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
applyCapacity w m       = w { capacity       = floor $ foldr (+) 1 [x | Capacity x <- m] * fromIntegral (capacity w) }
applyCritChance w m     = w { critChance     = sum1 [x | CritChance x <- m] * critChance w }
applyCritMultiplier w m = w { critMultiplier = sum1 [x | CritMultiplier x <- m] * critMultiplier w }
applyFireRate w m       = w { fireRate       = sum1 [x | FireRate x <- m] * fireRate w }
applyMagazine w m       = w { magazine       = floor $ sum1 [x | MagazineCapacity x <- m] * fromIntegral (magazine w) }
applyMultishot w m      = w { multishot      = sum [x | Multishot x <- m] + multishot w }
applyReload w m         = w { reload         = sum1 [x | Reload x <- m] * reload w }
applyStatus w m         = w { status         = sum1 [x | Status x <- m] * status w }

applyMods :: Weapon -> [Mod] -> Weapon
applyMods w m = foldl (\x y -> y x v) w fns
    where
        v   = concatMap modValues m
        fns = [applyDamage, applyAccuracy, applyCapacity, applyCritChance, applyCritMultiplier, applyFireRate, applyMagazine, applyMultishot, applyReload, applyStatus]

critDamage :: Weapon -> [Damage]
critDamage w = [Damage (a * m) t | Damage a t <- damage w]
    where
        m = critMultiplier w

-- Incorrect, the status-chance is a weighted distribution depending on the total damage
averageStatusDamage :: Weapon -> Float
averageStatusDamage w = sum [a * 2 | Damage a _ <- damage w] / fromIntegral (length $ damage w)

averageShotDamage :: Weapon -> [Damage]
averageShotDamage w = [Damage (a * m * c * s) t | Damage a t <- damage w]
    where
        m = 1 + multishot w
        c = 1 + critChance w * critMultiplier w
        -- TODO: Incorrect, currently just adding an incorrect status damage (2x total)
        s = 1 + status w

effectiveFireRate :: Weapon -> Float
effectiveFireRate w = m * f / (m + r * f)
    -- shots * (shots / second) / ( shots + seconds * (shots / second))
    where
        m = fromIntegral $ magazine w  -- shots
        f = fireRate w                 -- shots/second
        r = reload w                   -- seconds

damagePerSecond :: Weapon -> Float
damagePerSecond w = s * effectiveFireRate w
    where
        s = sum [a | Damage a _ <- averageShotDamage w]
