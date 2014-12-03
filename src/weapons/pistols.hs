module Weapons.Pistols where

import Weapon
import Damage

despair :: Weapon
despair = Weapon {
    accuracy=1.0,
    capacity=210,
    critChance=0.025,
    critMultiplier=1.5,
    damage=[Damage 2 Impact, Damage 44 Puncture, Damage 8 Slash],
    fireRate=3.3,
    magazine=10,
    multishot=0,
    name="Despair",
    reload=0.8,
    status=0.025,
    weaponType=Pistol
    }
embolist :: Weapon
embolist = Weapon {
    accuracy=1.0,
    capacity=210,
    critChance=0.025,
    critMultiplier=2,
    damage=[Damage 18.5 Toxic],
    fireRate=10,
    magazine=10,
    multishot=0,
    name="Embolist",
    reload=1.5,
    status=0.01,
    weaponType=Pistol
    }
hikouPrime :: Weapon
hikouPrime = Weapon {
    accuracy=1.0,
    capacity=210,
    critChance=0.05,
    critMultiplier=1.5,
    damage=[Damage 3.2 Impact, Damage 27.2 Puncture, Damage 1.6 Slash],
    fireRate=5.8,
    magazine=26,
    multishot=0,
    name="Hikou Prime",
    reload=0.5,
    status=0.15,
    weaponType=Pistol
    }
lexPrime :: Weapon
lexPrime = Weapon {
    accuracy=0.16,
    capacity=210,
    critChance=0.20,
    critMultiplier=2.0,
    damage=[Damage 8.5 Impact, Damage 68.0 Puncture, Damage 8.5 Slash],
    fireRate=2.1,
    magazine=8,
    multishot=0,
    name="Lex Prime",
    reload=2.3,
    status=0.20,
    weaponType=Pistol
    }
synoidGammacor :: Weapon
synoidGammacor = Weapon {
    accuracy=1.0,
    capacity=375,
    critChance=0.1,
    critMultiplier=2,
    damage=[Damage 210 Magnetic],
    fireRate=2,
    magazine=75,
    multishot=0,
    name="Synoid Gammacor",
    reload=2,
    status=0.2,
    weaponType=Pistol
    }
wraithTwinVipers :: Weapon
wraithTwinVipers = Weapon {
    accuracy=0.111,
    capacity=210,
    critChance=0.18,
    critMultiplier=2.0,
    damage=[Damage 14.4 Impact, Damage 1.8 Puncture, Damage 1.8 Slash],
    fireRate=25.0,
    magazine=40,
    multishot=0,
    name="Wraith Twin Vipers",
    reload=2.0,
    status=0.05,
    weaponType=Pistol
    }
vaykorMarelok :: Weapon
vaykorMarelok = Weapon {
    accuracy=0.1,
    capacity=210,
    critChance=0.2,
    critMultiplier=1.5,
    damage=[Damage 48 Slash, Damage 16 Puncture, Damage 96 Impact],
    fireRate=2,
    magazine=10,
    multishot=0,
    name="Vaykor Marelok",
    reload=1.7,
    status=0.35,
    weaponType=Pistol
    }
