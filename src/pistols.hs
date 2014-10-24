module Pistols where

import Weapon
import Damage
import Mod

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
    status=0.025
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
    status=0.01
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
    status=0.15
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
    status=0.20
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
    status=0.05
    }

mods :: [Mod]
mods = [
    Mod "Barrel Diffusion"  [Multishot 1.2],
    Mod "Concussion Rounds" [ElementalDamage 0.6 Impact],
    Mod "Convulsion"        [ElementalDamage 0.9 Electricity],
    Mod "Deep Freeze"       [ElementalDamage 0.6 Cold],
    Mod "Frostbite"         [ElementalDamage 0.6 Cold, Status 0.6],
    Mod "Gunslinger"        [FireRate 0.72],
    Mod "Heated Charge"     [ElementalDamage 0.9 Heat],
    Mod "Hollow Point"      [CritMultiplier 0.6, AnyDamage (-0.15)],
    Mod "Hornet Strike"     [AnyDamage 2.2],
    Mod "Ice Storm"         [MagazineCapacity 0.4, ElementalDamage 0.4 Cold],
    Mod "Jolt"              [ElementalDamage 0.6 Electricity, Status 0.6],
    Mod "Lethal Torrent"    [FireRate 0.6, Multishot 0.6],
    Mod "Magnum Force"      [AnyDamage 0.66, Accuracy (-0.33)],
    Mod "No Return"         [ElementalDamage 0.6 Puncture],
    Mod "Pathogen Rounds"   [ElementalDamage 0.9 Toxic],
    Mod "Pistol Gambit"     [CritChance 1.2],
    Mod "Pistol Pestilence" [ElementalDamage 0.6 Toxic, Status 0.6],
    Mod "Quickdraw"         [ReloadSpeed 0.48],
    Mod "Razor Shot"        [ElementalDamage 0.6 Slash],
    Mod "Scorch"            [ElementalDamage 0.6 Heat, Status 0.6],
    Mod "Slip Magazine"     [MagazineCapacity 0.3],
    Mod "Stunning Speed"    [ReloadSpeed 0.4, Status 0.1],
    Mod "Sure Shot"         [Status 0.15],
    Mod "Tainted Clip"      [MagazineCapacity 0.6, ReloadSpeed (-0.3)],
    Mod "Target Cracker"    [CritMultiplier 0.6],
    Mod "Trick Mag"         [Capacity 0.9]
    ]
