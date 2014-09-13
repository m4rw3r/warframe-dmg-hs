import Data.List
import Math.Combinat.Sets (choose)

data DamageType = Impact | Puncture | Slash | Heat | Cold | Electricity | Toxic | Blast | Corrosive | Gas | Magnetic | Radiation | Viral
    deriving (Eq, Ord, Enum, Read, Show, Bounded)

data Damage = Damage Float DamageType
    deriving (Show, Eq)

data ModValue =
    Accuracy Float
    | AnyDamage Float
    | Capacity Float
    | CritChance Float
    | CritMultiplier Float
    | ElementalDamage Float DamageType
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
        weaponName     :: String,
        reload         :: Float,
        status         :: Float
    }
    deriving (Show)

physicalDamageTypes  :: [DamageType]
elementalDamageTypes :: [DamageType]
physicalDamageTypes  = [Impact, Puncture, Slash]
elementalDamageTypes = [Heat, Cold, Electricity, Toxic]

sumByDamageType :: [Damage] -> [Damage]
sumByDamageType d = [Damage (sum [a | Damage a b <- d, b == t]) t | t <- types]
    where
        types = nub [t | Damage _ t <- d]

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
                                t `elem` elementalDamageTypes]
        -- Only apply the physical damage enhancing mods to the base value of that
        -- damage type. They do not scale with the total damage of the weapon.
        -- They are also applied simultaneously as the elemental damages, causing
        -- them to not affect elemental damage like toxic or heat.
        p = [Damage (a * c) t | ElementalDamage a t <- m,
                                Damage c t'         <- b,
                                t `elem` physicalDamageTypes,
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
        c = 1 + (critChance w) * (critMultiplier w)
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

lanka  :: Weapon
vulkar :: Weapon
lanka = Weapon {
    accuracy=1.0,
    capacity=72,
    critChance=0.25,
    critMultiplier=2.0,
    damage=[Damage 300 Electricity],
    fireRate=0.7,
    magazine=10,
    multishot=0,
    weaponName="Lanka",
    reload=2.0,
    status=0.25
    }
vulkar = Weapon {
    accuracy=0.133,
    capacity=72,
    critChance=0.20,
    critMultiplier=2.0,
    damage=[Damage 160 Impact, Damage 30 Puncture, Damage 10 Slash],
    fireRate=1.5,
    magazine=6,
    multishot=0,
    weaponName="Vulkar",
    reload=3.0,
    status=0.25
    }

rifleMods :: [Mod]
rifleMods = [
    Mod "Ammo Drum"       [Capacity 0.3],
    Mod "Critical Delay"  [CritChance 0.48, FireRate (-0.36)],
    Mod "Cryo Rounds"     [ElementalDamage 0.9 Cold],
    Mod "Fast Hands"      [Reload (-0.3)],
    Mod "Hammershot"      [CritMultiplier 0.6, Status 0.4],
    Mod "Heavy Caliber"   [AnyDamage 1.65, Accuracy (-0.5)],
    Mod "Hellfire"        [ElementalDamage 0.9 Heat],
    Mod "High Voltage"    [ElementalDamage 0.6 Electricity, Status 0.6],
    Mod "Infected Clip"   [ElementalDamage 0.9 Toxic],
    Mod "Magazine Warp"   [MagazineCapacity 0.3],
    Mod "Malignant Force" [ElementalDamage 0.6 Toxic, Status 0.6],
    Mod "Piercing Hit"    [ElementalDamage 0.3 Puncture],
    Mod "Point Strike"    [CritChance 1.5],
    Mod "Rifle Aptitude"  [Status 0.15],
    Mod "Rime Rounds"     [ElementalDamage 0.6 Cold, Status 0.6],
    Mod "Rupture"         [ElementalDamage 0.3 Impact],
    Mod "Sawtooth Clip"   [ElementalDamage 0.3 Slash],
    Mod "Serration"       [AnyDamage 1.65],
    Mod "Shred"           [FireRate 0.3], -- Add punch-through
    Mod "Speed Trigger"   [FireRate 0.6],
    Mod "Split Chamber"   [Multishot 0.9],
    Mod "Stormbringer"    [ElementalDamage 0.9 Electricity],
    Mod "Tainted Mag"     [MagazineCapacity 0.66, Reload 0.33],
    Mod "Thermite Rounds" [ElementalDamage 0.6 Heat, Status 0.6],
    Mod "Vital Sense"     [CritMultiplier 1.2],
    Mod "Wildfire"        [MagazineCapacity 0.2, ElementalDamage 0.6 Heat]
    ]

mods :: [Mod]
mods = modsByNames ["Serration", "Speed Trigger", "Split Chamber", "Heavy Caliber", "Stormbringer", "Infected Clip", "Point Strike", "Vital Sense"] rifleMods

combos :: [[Mod]]
combos = choose 8 rifleMods

findMaximum :: (a -> Float) -> (Float, a) -> a -> (Float, a)
findMaximum f (av, a) b = if bv > av then (bv, b) else (av, a)
    where
        bv = f b

cmpFst :: Ord a => (a, b) -> (a, b) -> Ordering
cmpFst (a, _) (b, _) = compare a b

findMaximumN :: Int -> (a -> Float) -> [(Float, a)] -> a -> [(Float, a)]
findMaximumN n f as b = take n vs
    where
        vs = sortBy (flip cmpFst) (as ++ [(f b, b)])

main :: IO ()
main = print "test"
    >> print physicalDamageTypes
    >> print lanka
    >> print mods
    >> print (applyMods lanka mods)
    >> print (applyMods vulkar mods)
