module Rifles where

import Weapon
import Damage
import Mod

dread :: Weapon
dread = Weapon {
    accuracy=1.0,
    capacity=72,
    critChance=0.50,
    critMultiplier=2.0,
    damage=[Damage 10 Impact, Damage 10 Puncture, Damage 180 Slash],
    fireRate=1.0,
    magazine=1,
    multishot=0,
    name="Dread",
    reload=1.0,
    status=0.20
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
    status=0.25
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
    status=0.25
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
    status=0.30
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
    status=0.25
    }

mods :: [Mod]
mods = [
    Mod "Ammo Drum"       [Capacity 0.3],
    Mod "Critical Delay"  [CritChance 0.48, FireRate (-0.36)],
    Mod "Cryo Rounds"     [ElementalDamage 0.9 Cold],
    Mod "Fast Hands"      [ReloadSpeed 0.3],
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
    Mod "Tainted Mag"     [MagazineCapacity 0.66, ReloadSpeed (-0.33)],
    Mod "Thermite Rounds" [ElementalDamage 0.6 Heat, Status 0.6],
    Mod "Vital Sense"     [CritMultiplier 1.2],
    Mod "Wildfire"        [MagazineCapacity 0.2, ElementalDamage 0.6 Heat]
    ]
