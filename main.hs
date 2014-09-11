import Data.List

data DamageType = AnyDamage | Impact | Puncture | Slash | Heat | Cold | Electricity | Toxic | Blast | Corrosive | Gas | Magnetic | Radiation | Viral
	deriving (Eq, Ord, Enum, Read, Show, Bounded)

data Damage = Damage Float DamageType
	deriving (Show, Eq)

instance Num Damage where
	Damage a c + Damage b c' | c == c' = Damage (a + b) c
	Damage a c * Damage b c' | c == c' = Damage (a * b) c
	Damage a c * Damage b AnyDamage = Damage (a * b) c
	abs (Damage a b) = Damage (abs a) b
	signum (Damage a c) = Damage (signum a) c

data ModValue = Capacity Float
	| CritChance Float
	| FireRate Float
	| Reload Float
	| CritMultiplier Float
	| Status Float
	| ModDamage Damage
	| Multishot Float
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

physicalDamageTypes = [Impact, Puncture, Slash]
elementalDamageTypes = [Heat, Cold, Electricity, Toxic]

applyBaseDamage :: Weapon -> [Mod] -> Weapon
applyBaseDamage w m = w { damage = map (* d) (damage w) }
    where
        d = Damage (foldr (+) 1 [a | ModDamage (Damage a AnyDamage) <- concat m]) AnyDamage

isPhysicalDamage :: Damage -> Bool
isPhysicalDamage (Damage _ t) = elem t physicalDamageTypes
isPhysicalDamage _ = False

isElementalDamage :: Damage -> Bool
isElementalDamage (Damage _ t) = elem t elementalDamageTypes
isElementalDamage _ = False

sumDamageType :: DamageType -> [Damage] -> Damage
sumDamageType t d = Damage (foldr (+) 0 [a | Damage a b <- d, b == t]) t

sumDamageTypes :: [Damage] -> [Damage]
sumDamageTypes d = [sumDamageType t d | t <- nub [t | Damage _ t <- d]]

modDamage :: [Mod] -> [Damage]
modDamage m = [Damage a b | ModDamage (Damage a b) <- concat m]

applyElementalDamage :: Weapon -> [Mod] -> Weapon
applyElementalDamage w m = w { damage = sumDamageTypes (concat [(damage w), e, p]) }
    where
        t = (foldr (+) 0 [a | Damage a _ <- damage w])
        e = [Damage (a * t) b | Damage a b <- filter isElementalDamage (modDamage m)]
        p = [Damage (a * b) c | Damage a c <- filter isPhysicalDamage (modDamage m), Damage b c' <- filter isPhysicalDamage (damage w), c == c']

applyDamage w m = applyElementalDamage (applyBaseDamage w m) m

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

serration    = [ModDamage (Damage 1.65 AnyDamage)]
splitChamber = [Multishot 0.9]
stormbringer = [ModDamage (Damage 0.9 Electricity)]
infectedClip = [ModDamage (Damage 0.9 Toxic)]
piercingHit  = [ModDamage (Damage 0.3 Puncture)]
sawtoothClip = [ModDamage (Damage 0.3 Slash)]
rupture      = [ModDamage (Damage 0.3 Impact)]

mods = [serration, splitChamber, stormbringer, infectedClip, rupture]

main :: IO ()
main = do print "test"
	>> print physicalDamageTypes
	>> print lanka
	>> print mods
	>> print (modDamage mods)
	>> print [a | ModDamage (Damage a AnyDamage) <- concat mods]
	>> print (applyBaseDamage lanka mods)
	>> print (filter isPhysicalDamage (modDamage mods))
	>> print [Damage (a * b) c | Damage a c <- filter isPhysicalDamage (modDamage mods), Damage b c' <- filter isPhysicalDamage (damage vulkar), c == c']
	>> print (applyElementalDamage vulkar mods)
	>> print (applyDamage vulkar mods)
