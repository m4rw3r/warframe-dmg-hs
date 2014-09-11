import Data.List

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
    | Multishot Float
    | Reload Float
    | Status Float
        deriving (Show, Eq)

type Mod = [ModValue]

data Weapon =
    Weapon {
        accuracy       :: Float,
        critChance     :: Float,
        critMultiplier :: Float,
        damage         :: [Damage],
        fireRate       :: Float,
        magazine       :: Float,
        name           :: String,
        reload         :: Float,
        status         :: Float
    }
    deriving (Show)

physicalDamageTypes :: [DamageType]
physicalDamageTypes  = [Impact, Puncture, Slash]
elementalDamageTypes :: [DamageType]
elementalDamageTypes = [Heat, Cold, Electricity, Toxic]

applyBaseDamage :: Weapon -> [Mod] -> Weapon
applyBaseDamage w m = w { damage = [Damage (a * d) t | Damage a t <- damage w] }
    where
        d = foldr (+) 1 [a | AnyDamage a <- concat m]

sumByDamageType :: [Damage] -> [Damage]
sumByDamageType d = [Damage (sum [a | Damage a b <- d, b == t]) t | t <- types]
    where
        types = nub [t | Damage _ t <- d]

applyElementalDamage :: Weapon -> [Mod] -> Weapon
applyElementalDamage w m = w { damage = sumByDamageType (concat [damage w, e, p]) }
    where
        t = sum [a | Damage a _ <- damage w]
        -- Elemental damage is scaled to `total damage * elemental fraction`
        e = [Damage (a * t) b | ElementalDamage a b <- concat m,
                                b `elem` elementalDamageTypes]
        -- Only apply the physical damage enhancing mods to the base value of that
        -- damage type. They do not scale with the total damage of the weapon.
        -- They are also applied simultaneously as the elemental damages, causing
        -- them to not affect elemental damage like toxic or heat.
        p = [Damage (a * b) c | ElementalDamage a c <- concat m,
                                Damage b c'         <- damage w,
                                c `elem` physicalDamageTypes,
                                c == c']

applyDamage :: Weapon -> [Mod] -> Weapon
applyDamage w m = applyElementalDamage (applyBaseDamage w m) m

lanka  :: Weapon
vulkar :: Weapon
lanka = Weapon {
    accuracy=1.0,
    critChance=0.25,
    critMultiplier=2.0,
    damage=[Damage 300 Electricity],
    fireRate=0.7,
    magazine=10,
    name="Lanka",
    reload=2.0,
    status=0.25
    }
vulkar = Weapon {
    accuracy=0.133,
    critChance=0.20,
    critMultiplier=2.0,
    damage=[Damage 160 Impact, Damage 30 Puncture, Damage 10 Slash],
    fireRate=1.5,
    magazine=6,
    name="Vulkar",
    reload=3.0,
    status=0.25
    }

serration    :: Mod
splitChamber :: Mod
stormbringer :: Mod
infectedClip :: Mod
piercingHit  :: Mod
sawtoothClip :: Mod
rupture      :: Mod
serration    = [AnyDamage 1.65]
splitChamber = [Multishot 0.9]
stormbringer = [ElementalDamage 0.9 Electricity]
infectedClip = [ElementalDamage 0.9 Toxic]
piercingHit  = [ElementalDamage 0.3 Puncture]
sawtoothClip = [ElementalDamage 0.3 Slash]
rupture      = [ElementalDamage 0.3 Impact]

mods :: [Mod]
mods = [serration, splitChamber, stormbringer, infectedClip, rupture]

main :: IO ()
main = print "test"
    >> print physicalDamageTypes
    >> print lanka
    >> print mods
    >> print (applyBaseDamage lanka mods)
    >> print (applyElementalDamage vulkar mods)
    >> print (applyDamage vulkar mods)
