module Weapons.Rifles where

import Weapon
import Damage

amprex :: Weapon
amprex = Weapon {
    accuracy=1.0,
    capacity=540,
    critChance=0.5,
    critMultiplier=2.0,
    damage=[Damage 7.5 Electricity],
    fireRate=20,
    magazine=100,
    multishot=0,
    name="Amprex",
    reload=2.7,
    status=0.011,
    weaponType=Rifle
    }
boltorPrime :: Weapon
boltorPrime = Weapon {
    accuracy=0.5,
    capacity=540,
    critChance=0.05,
    critMultiplier=2.0,
    damage=[Damage 5.5 Impact, Damage 49.5 Puncture],
    fireRate=10.0,
    magazine=60,
    multishot=0,
    name="Boltor Prime",
    reload=2.4,
    status=0.10,
    weaponType=Rifle
    }
bratonPrime :: Weapon
bratonPrime = Weapon {
    accuracy=0.286,
    capacity=540,
    critChance=0.1,
    critMultiplier=1.5,
    damage=[Damage 1.3 Impact, Damage 8.8 Puncture, Damage 15 Slash],
    fireRate=8.8,
    magazine=50,
    multishot=0,
    name="Braton Prime",
    reload=2.2,
    status=0.1,
    weaponType=Rifle
    }
dera :: Weapon
dera = Weapon {
    accuracy=1,
    capacity=540,
    critChance=0.025,
    critMultiplier=1.5,
    damage=[Damage 1.1 Slash, Damage 16.5 Puncture, Damage 4.4 Impact],
    fireRate=11.3,
    magazine=45,
    multishot=0,
    name="Dera",
    reload=2.37,
    status=0.1,
    weaponType=Rifle
    }
dread :: Weapon
dread = Weapon {
    accuracy=1.0,
    capacity=72,
    critChance=0.50,
    critMultiplier=2.0,
    damage=[Damage 10 Impact, Damage 10 Puncture, Damage 180 Slash],
    fireRate=0.5,
    magazine=1,
    multishot=0,
    name="Dread",
    reload=0.8,
    status=0.20,
    weaponType=Bow
    }
glaxion :: Weapon
glaxion = Weapon {
    accuracy=0.125,
    capacity=1500,
    critChance=0.05,
    critMultiplier=2.0,
    damage=[Damage 12.5 Cold],
    fireRate=20.0,
    magazine=300,
    multishot=0,
    name="Glaxion",
    reload=1.5,
    status=0.04,
    weaponType=Rifle
    }
grakata :: Weapon
grakata = Weapon {
    accuracy=0.286,
    capacity=800,
    critChance=0.25,
    critMultiplier=2,
    damage=[Damage 2.9 Slash, Damage 3.7 Puncture, Damage 4.4 Impact],
    fireRate=20,
    magazine=60,
    multishot=0,
    name="Grakata",
    reload=2.4,
    status=0.2,
    weaponType=Rifle
    }
karak :: Weapon
karak = Weapon {
    accuracy=0.286,
    capacity=540,
    critChance=0.025,
    critMultiplier=1.5,
    damage=[Damage 6.8 Slash, Damage 8.1 Puncture, Damage 12.1 Impact],
    fireRate=11.7,
    magazine=30,
    multishot=0,
    name="Karak",
    reload=2.0,
    status=0.075,
    weaponType=Rifle
    }
lanka  :: Weapon
lanka = Weapon {
    accuracy=1.0,
    capacity=72,
    critChance=0.25,
    critMultiplier=2.0,
    damage=[Damage 300 Electricity],
    fireRate=0.7,
    magazine=10,
    multishot=0,
    name="Lanka",
    reload=2.0,
    status=0.25,
    weaponType=Sniper
    }
latron :: Weapon
latron = Weapon {
    accuracy=0.286,
    capacity=540,
    critChance=0.1,
    critMultiplier=2,
    damage=[Damage 8 Impact, Damage 38 Puncture, Damage 8 Slash],
    fireRate=4.2,
    magazine=15,
    multishot=0,
    name="Latron",
    reload=2.4,
    status=0.1,
    weaponType=Rifle
    }
latronWraith :: Weapon
latronWraith = Weapon {
    accuracy=0.286,
    capacity=540,
    critChance=0.25,
    critMultiplier=2.5,
    damage=[Damage 13.8 Impact, Damage 38.5 Puncture, Damage 2.7 Slash],
    fireRate=5.4,
    magazine=15,
    multishot=0,
    name="Latron Wraith",
    reload=2.4,
    status=0.20,
    weaponType=Rifle
    }
opticor :: Weapon
opticor = Weapon {
    accuracy=1.0,
    capacity=540,
    critChance=0.15,
    critMultiplier=2.0,
    damage=[Damage 50 Slash, Damage 850 Puncture, Damage 100 Impact],
    -- damage=[Damage 25 Slash, Damage 425 Puncture, Damage 50 Impact],
    fireRate=0.3,
    -- fireRate=0.6,
    magazine=5,
    multishot=0,
    name="Opticor",
    reload=2.0,
    status=0.15,
    weaponType=Rifle
    }
parisPrime :: Weapon
parisPrime = Weapon {
    accuracy=1.0,
    capacity=72,
    critChance=0.45,
    critMultiplier=2.0,
    damage=[Damage 5 Impact, Damage 160 Puncture, Damage 35 Slash],
    fireRate=0.5,
    magazine=1,
    multishot=0,
    name="Paris Prime",
    reload=0.7,
    status=0.20,
    weaponType=Bow
    }
quanta :: Weapon
quanta = Weapon {
    accuracy=1.0,
    capacity=540,
    critChance=0.10,
    critMultiplier=2.0,
    damage=[Damage 220 Electricity],
    fireRate=1.0,
    magazine=60,
    multishot=0,
    name="Quanta",
    reload=2.0,
    status=0.1,
    weaponType=Rifle
    }
quantaDetonate :: Weapon
quantaDetonate = Weapon {
    accuracy=1.0,
    capacity=540,
    critChance=0.10,
    critMultiplier=2.0,
    damage=[Damage 400 Blast],
    fireRate=1.0,
    magazine=5,
    multishot=0,
    name="Quanta",
    reload=2.0,
    status=0.1,
    weaponType=Rifle
    }
soma :: Weapon
soma = Weapon {
    accuracy=0.286,
    capacity=540,
    critChance=0.3,
    critMultiplier=3,
    damage=[Damage 5 Slash, Damage 4 Puncture, Damage 1 Impact],
    fireRate=15,
    magazine=100,
    multishot=0,
    name="Soma",
    reload=3,
    status=0.07,
    weaponType=Rifle
    }
snipetronVandal  :: Weapon
snipetronVandal = Weapon {
    accuracy=0.133,
    capacity=72,
    critChance=0.25,
    critMultiplier=2.0,
    damage=[Damage 7.5 Impact, Damage 135 Puncture, Damage 7.5 Slash],
    fireRate=1.5,
    magazine=6,
    multishot=0,
    name="Snipetron Vandal",
    reload=2.0,
    status=0.25,
    weaponType=Sniper
    }
synapse  :: Weapon
synapse = Weapon {
    accuracy=0.125,
    capacity=540,
    critChance=0.50,
    critMultiplier=2.0,
    damage=[Damage 12.5 Electricity],
    fireRate=10,
    magazine=100,
    multishot=0,
    name="Synapse",
    reload=1.0,
    status=0.025,
    weaponType=Rifle
    }
tiberion :: Weapon
tiberion = Weapon {
    accuracy=0.333,
    capacity=540,
    critChance=0.05,
    critMultiplier=2,
    damage=[Damage 15 Slash, Damage 30 Puncture, Damage 15 Impact],
    fireRate=6.7,
    magazine=30,
    multishot=0,
    name="Tiberion",
    reload=2.3,
    status=0.025,
    weaponType=Rifle
    }
vectis :: Weapon
vectis = Weapon {
    accuracy=0.133,
    capacity=72,
    critChance=0.25,
    critMultiplier=2.0,
    damage=[Damage 90 Impact, Damage 78.8 Puncture, Damage 56.3 Slash],
    fireRate=1.5,
    magazine=1,
    multishot=0,
    name="Vectis",
    reload=0.9,
    status=0.30,
    weaponType=Sniper
    }
vulkar :: Weapon
vulkar = Weapon {
    accuracy=0.133,
    capacity=72,
    critChance=0.20,
    critMultiplier=2.0,
    damage=[Damage 160 Impact, Damage 30 Puncture, Damage 10 Slash],
    fireRate=1.5,
    magazine=6,
    multishot=0,
    name="Vulkar",
    reload=3.0,
    status=0.25,
    weaponType=Sniper
    }
