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
