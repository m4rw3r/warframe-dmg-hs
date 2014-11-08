module Weapons.Shotguns where

import Weapon
import Damage

boarPrime :: Weapon
boarPrime = Weapon {
    accuracy=0,
    capacity=120,
    critChance=0.15,
    critMultiplier=2,
    damage=[Damage 23.4 Slash, Damage 17.6 Puncture, Damage 76 Impact],
    fireRate=5.8,
    magazine=15,
    multishot=0,
    name="Boar Prime",
    reload=2.3,
    status=0.4,
    weaponType=Shotgun
    }
drakgoonCharged :: Weapon
drakgoonCharged = Weapon {
    accuracy=0.014,
    capacity=120,
    critChance=0.075,
    critMultiplier=2.0,
    damage=[Damage 25 Impact, Damage 25 Puncture, Damage 200 Slash],
    fireRate=1.25, -- charged
    magazine=7,
    multishot=0,
    name="Drakgoon Charged",
    reload=2.3,
    status=0.10,
    weaponType=Shotgun
    }
drakgoonUncharged :: Weapon
drakgoonUncharged = Weapon {
    accuracy=0.014,
    capacity=120,
    critChance=0.075,
    critMultiplier=2.0,
    damage=[Damage 25 Impact, Damage 25 Puncture, Damage 200 Slash],
    fireRate=3.6, -- uncharged
    magazine=7,
    multishot=0,
    name="Drakgoon Uncharged",
    reload=2.3,
    status=0.10,
    weaponType=Shotgun
    }
phage :: Weapon
phage = Weapon {
    accuracy=0.5,
    capacity=120,
    critChance=0.075,
    critMultiplier=2.0,
    damage=[Damage 330 Viral],
    fireRate=1.0,
    magazine=40,
    multishot=0,
    name="Phage",
    reload=2.0,
    status=0.10,
    weaponType=Shotgun
    }
