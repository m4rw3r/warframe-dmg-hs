module Mods (modsFor) where

import Damage
import Mod
import Weapon
import WeaponType

conditionAppliesOn :: Weapon -> ModRequirement -> Bool
conditionAppliesOn w (ModType t) = case (t, weaponType w) of
    (Rifle, Sniper) -> True
    (Rifle, Bow)    -> True
    (a, b)          -> a == b
conditionAppliesOn w (WeaponName n) = n == name w

canApplyOn :: Weapon -> Mod -> Bool
canApplyOn w (Mod _ r _) = all (conditionAppliesOn w) r

modsFor :: Weapon -> [Mod]
modsFor w = filter (canApplyOn w) mods

mods :: [Mod]
mods = [
    -- Pistols
    Mod "Barrel Diffusion"  [ModType Pistol] [Multishot 1.2],
    Mod "Concussion Rounds" [ModType Pistol] [ElementalDamage 0.6 Impact],
    Mod "Convulsion"        [ModType Pistol] [ElementalDamage 0.9 Electricity],
    Mod "Deep Freeze"       [ModType Pistol] [ElementalDamage 0.6 Cold],
    Mod "Frostbite"         [ModType Pistol] [ElementalDamage 0.6 Cold, Status 0.6],
    Mod "Gunslinger"        [ModType Pistol] [FireRate 0.72],
    Mod "Heated Charge"     [ModType Pistol] [ElementalDamage 0.9 Heat],
    Mod "Hollow Point"      [ModType Pistol] [CritMultiplier 0.6, AnyDamage (-0.15)],
    Mod "Hornet Strike"     [ModType Pistol] [AnyDamage 2.2],
    Mod "Ice Storm"         [ModType Pistol] [MagazineCapacity 0.4, ElementalDamage 0.4 Cold],
    Mod "Jolt"              [ModType Pistol] [ElementalDamage 0.6 Electricity, Status 0.6],
    Mod "Lethal Torrent"    [ModType Pistol] [FireRate 0.6, Multishot 0.6],
    Mod "Magnum Force"      [ModType Pistol] [AnyDamage 0.66, Accuracy (-0.33)],
    Mod "No Return"         [ModType Pistol] [ElementalDamage 0.6 Puncture],
    Mod "Pathogen Rounds"   [ModType Pistol] [ElementalDamage 0.9 Toxic],
    Mod "Pistol Gambit"     [ModType Pistol] [CritChance 1.2],
    Mod "Pistol Pestilence" [ModType Pistol] [ElementalDamage 0.6 Toxic, Status 0.6],
    Mod "Quickdraw"         [ModType Pistol] [ReloadSpeed 0.48],
    Mod "Razor Shot"        [ModType Pistol] [ElementalDamage 0.6 Slash],
    Mod "Scorch"            [ModType Pistol] [ElementalDamage 0.6 Heat, Status 0.6],
    Mod "Slip Magazine"     [ModType Pistol] [MagazineCapacity 0.3],
    Mod "Stunning Speed"    [ModType Pistol] [ReloadSpeed 0.4, Status 0.1],
    Mod "Sure Shot"         [ModType Pistol] [Status 0.15],
    Mod "Tainted Clip"      [ModType Pistol] [MagazineCapacity 0.6, ReloadSpeed (-0.3)],
    Mod "Target Cracker"    [ModType Pistol] [CritMultiplier 0.6],
    Mod "Trick Mag"         [ModType Pistol] [Capacity 0.9],

    -- Rifles
    Mod "Ammo Drum"         [ModType Rifle] [Capacity 0.3],
    Mod "Critical Delay"    [ModType Rifle] [CritChance 0.48, FireRate (-0.36)],
    Mod "Cryo Rounds"       [ModType Rifle] [ElementalDamage 0.9 Cold],
    Mod "Fast Hands"        [ModType Rifle] [ReloadSpeed 0.3],
    Mod "Fanged Fusilade"   [ModType Rifle] [ElementalDamage 1.2 Slash],
    Mod "Hammershot"        [ModType Rifle] [CritMultiplier 0.6, Status 0.4],
    Mod "Heavy Caliber"     [ModType Rifle] [AnyDamage 1.65, Accuracy (-0.5)],
    Mod "Hellfire"          [ModType Rifle] [ElementalDamage 0.9 Heat],
    Mod "High Voltage"      [ModType Rifle] [ElementalDamage 0.6 Electricity, Status 0.6],
    Mod "Infected Clip"     [ModType Rifle] [ElementalDamage 0.9 Toxic],
    Mod "Magazine Warp"     [ModType Rifle] [MagazineCapacity 0.3],
    Mod "Malignant Force"   [ModType Rifle] [ElementalDamage 0.6 Toxic, Status 0.6],
    Mod "Piercing Hit"      [ModType Rifle] [ElementalDamage 0.3 Puncture],
    Mod "Point Strike"      [ModType Rifle] [CritChance 1.5],
    Mod "Rifle Aptitude"    [ModType Rifle] [Status 0.15],
    Mod "Rime Rounds"       [ModType Rifle] [ElementalDamage 0.6 Cold, Status 0.6],
    Mod "Rupture"           [ModType Rifle] [ElementalDamage 0.3 Impact],
    Mod "Sawtooth Clip"     [ModType Rifle] [ElementalDamage 0.3 Slash],
    Mod "Serration"         [ModType Rifle] [AnyDamage 1.65],
    Mod "Shred"             [ModType Rifle] [FireRate 0.3], -- Add punch-through
    Mod "Speed Trigger"     [ModType Rifle] [FireRate 0.6],
    Mod "Split Chamber"     [ModType Rifle] [Multishot 0.9],
    Mod "Stormbringer"      [ModType Rifle] [ElementalDamage 0.9 Electricity],
    Mod "Tainted Mag"       [ModType Rifle] [MagazineCapacity 0.66, ReloadSpeed (-0.33)],
    Mod "Thermite Rounds"   [ModType Rifle] [ElementalDamage 0.6 Heat, Status 0.6],
    Mod "Vital Sense"       [ModType Rifle] [CritMultiplier 1.2],
    Mod "Vile Acceleration" [ModType Rifle] [FireRate 0.9, AnyDamage (-0.125)],
    Mod "Wildfire"          [ModType Rifle] [MagazineCapacity 0.2, ElementalDamage 0.6 Heat],

    -- Shotguns
    Mod "Accelerated Blast"  [ModType Shotgun] [FireRate 0.6, ElementalDamage 0.6 Puncture],
    Mod "Ammo Stock"         [ModType Shotgun] [MagazineCapacity 0.6],
    Mod "Blaze"              [ModType Shotgun] [AnyDamage 0.6, ElementalDamage 0.6 Heat],
    Mod "Blunderbuss"        [ModType Shotgun] [CritChance 0.9],
    Mod "Burdened Magazine"  [ModType Shotgun] [MagazineCapacity 0.6, ReloadSpeed (-0.18)],
    Mod "Charged Shell"      [ModType Shotgun] [ElementalDamage 0.9 Electricity],
    Mod "Chilling Grasp"     [ModType Shotgun] [ElementalDamage 0.9 Cold],
    Mod "Contagious Spread"  [ModType Shotgun] [ElementalDamage 0.9 Toxic],
    Mod "Disruptor"          [ModType Shotgun] [ElementalDamage 0.3 Impact],
    Mod "Flechette"          [ModType Shotgun] [ElementalDamage 0.3 Puncture],
    Mod "Frigid Blast"       [ModType Shotgun] [ElementalDamage 0.6 Cold, Status 0.6],
    Mod "Hell's Chamber"     [ModType Shotgun] [Multishot 1.2],
    Mod "Incendiary Coat"    [ModType Shotgun] [ElementalDamage 0.9 Heat],
    Mod "Point Blank"        [ModType Shotgun] [AnyDamage 0.9],
    Mod "Ravage"             [ModType Shotgun] [CritMultiplier 0.6],
    Mod "Scattered Justice"  [ModType Shotgun, WeaponName "Hek"] [Multishot 2],
    Mod "Scattering Inferno" [ModType Shotgun] [ElementalDamage 0.6 Heat, Status 0.6],
    Mod "Shell Shock"        [ModType Shotgun] [ElementalDamage 0.6 Electricity, Status 0.6],
    Mod "Shotgun Savvy"      [ModType Shotgun] [Status 0.3],
    Mod "Shotgun Spazz"      [ModType Shotgun] [FireRate 0.9],
    Mod "Shredder"           [ModType Shotgun] [ElementalDamage 0.3 Slash],
    Mod "Tactical Pump"      [ModType Shotgun] [ReloadSpeed 0.3],
    Mod "Tainted Shell"      [ModType Shotgun] [FireRate (-0.66)], -- -spread
    Mod "Toxic Barrage"      [ModType Shotgun] [ElementalDamage 0.6 Toxic, Status 0.6],
    Mod "Vicious Spread"     [ModType Shotgun] [AnyDamage 0.9] -- +spread
    ]
