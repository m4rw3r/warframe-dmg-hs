module Shotguns where

import Weapon
import Damage
import Mod

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
    status=0.10
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
    status=0.10
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
    status=0.10
    }

mods :: [Mod]
mods = [
    Mod "Accelerated Blast" [FireRate 0.6, ElementalDamage 0.6 Puncture],
    Mod "Ammo Stock" [MagazineCapacity 0.6],
    Mod "Blaze" [AnyDamage 0.6, ElementalDamage 0.6 Heat],
    Mod "Blunderbuss" [CritChance 0.9],
    Mod "Burdened Magazine" [MagazineCapacity 0.6, ReloadSpeed (-0.18)],
    Mod "Charged Shell" [ElementalDamage 0.9 Electricity],
    Mod "Chilling Grasp" [ElementalDamage 0.9 Cold],
    Mod "Contagious Spread" [ElementalDamage 0.9 Toxic],
    Mod "Disruptor" [ElementalDamage 0.3 Impact],
    Mod "Flechette" [ElementalDamage 0.3 Puncture],
    Mod "Frigid Blast" [ElementalDamage 0.6 Cold, Status 0.6],
    Mod "Hell's Chamber" [Multishot 1.2],
    Mod "Incendiary Coat" [ElementalDamage 0.9 Heat],
    Mod "Point Blank" [AnyDamage 0.9],
    Mod "Ravage" [CritMultiplier 0.6],
    Mod "Scattering Inferno" [ElementalDamage 0.6 Heat, Status 0.6],
    Mod "Shell Shock" [ElementalDamage 0.6 Electricity, Status 0.6],
    Mod "Shotgun Savvy" [Status 0.3],
    Mod "Shotgun Spazz" [FireRate 0.9],
    Mod "Shredder" [ElementalDamage 0.3 Slash],
    Mod "Tactical Pump" [ReloadSpeed 0.3],
    Mod "Tainted Shell" [FireRate (-0.66)], -- -spread
    Mod "Toxic Barrage" [ElementalDamage 0.6 Toxic, Status 0.6],
    Mod "Vicious Spread" [AnyDamage 0.9] -- +spread
    ]
