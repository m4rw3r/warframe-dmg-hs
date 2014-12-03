module Armor where

import qualified Damage

data Armor =
      Alloy
    | ClonedFlesh
    | Ferrite
    | Flesh
    | Fossilized
    | Infested
    | InfestedFlesh
    | Machinery
    | ProtoShield
    | Robotic
    | Shield
    | Sinew
        deriving (Eq, Show)

-- |ArmorValue is armor which is applied to the weapon, it has a type
-- and a base-armor, Health-based armors have a base-armor set to 0.
data ArmorValue = ArmorValue Armor Int
    deriving (Eq, Show)

data Enemy = Enemy [ArmorValue] Int
    deriving (Eq, Show)

modifierForDamageOnArmor :: Damage.Type -> Armor -> Float
modifierForDamageOnArmor Damage.Impact      Flesh         = -0.25
modifierForDamageOnArmor Damage.Impact      Machinery     =  0.25
modifierForDamageOnArmor Damage.Impact      Shield        =  0.5
modifierForDamageOnArmor Damage.Impact      ProtoShield   =  0.25
modifierForDamageOnArmor Damage.Puncture    InfestedFlesh =  0.25
modifierForDamageOnArmor Damage.Puncture    Sinew         =  0.25
modifierForDamageOnArmor Damage.Puncture    Robotic       =  0.25
modifierForDamageOnArmor Damage.Puncture    Shield        = -0.25
modifierForDamageOnArmor Damage.Puncture    ProtoShield   = -0.5
modifierForDamageOnArmor Damage.Puncture    Ferrite       =  0.5
modifierForDamageOnArmor Damage.Puncture    Alloy         =  0.15
modifierForDamageOnArmor Damage.Slash       Flesh         =  0.25
modifierForDamageOnArmor Damage.Slash       ClonedFlesh   =  0.25
modifierForDamageOnArmor Damage.Slash       Fossilized    =  0.15
modifierForDamageOnArmor Damage.Slash       Infested      =  0.5
modifierForDamageOnArmor Damage.Slash       Robotic       = -0.25
modifierForDamageOnArmor Damage.Slash       Ferrite       = -0.25
modifierForDamageOnArmor Damage.Slash       Alloy         = -0.5
modifierForDamageOnArmor Damage.Cold        Fossilized    = -0.25
modifierForDamageOnArmor Damage.Cold        Infested      = -0.5
modifierForDamageOnArmor Damage.Cold        InfestedFlesh =  0.25
modifierForDamageOnArmor Damage.Cold        Sinew         =  0.25
modifierForDamageOnArmor Damage.Cold        Shield        =  0.5
modifierForDamageOnArmor Damage.Cold        Alloy         =  0.5
modifierForDamageOnArmor Damage.Electricity Machinery     =  0.5
modifierForDamageOnArmor Damage.Electricity Robotic       =  0.5
modifierForDamageOnArmor Damage.Electricity Alloy         = -0.5
modifierForDamageOnArmor Damage.Heat        ClonedFlesh   =  0.25
modifierForDamageOnArmor Damage.Heat        Infested      =  0.5
modifierForDamageOnArmor Damage.Heat        ProtoShield   = -0.5
modifierForDamageOnArmor Damage.Toxic       Flesh         =  0.5
modifierForDamageOnArmor Damage.Toxic       Fossilized    = -0.5
modifierForDamageOnArmor Damage.Toxic       Machinery     = -0.25
modifierForDamageOnArmor Damage.Toxic       Robotic       = -0.25
modifierForDamageOnArmor Damage.Toxic       Shield        =  0 -- Bypass
modifierForDamageOnArmor Damage.Toxic       ProtoShield   =  0.25
modifierForDamageOnArmor Damage.Toxic       Ferrite       =  0.25
modifierForDamageOnArmor Damage.Blast       Fossilized    =  0.5
modifierForDamageOnArmor Damage.Blast       InfestedFlesh = -0.5
modifierForDamageOnArmor Damage.Blast       Sinew         = -0.5
modifierForDamageOnArmor Damage.Blast       Machinery     =  0.75
modifierForDamageOnArmor Damage.Blast       Ferrite       = -0.25
modifierForDamageOnArmor Damage.Corrosive   Fossilized    =  0.75
modifierForDamageOnArmor Damage.Corrosive   ProtoShield   = -0.5
modifierForDamageOnArmor Damage.Corrosive   Ferrite       =  0.75
modifierForDamageOnArmor Damage.Gas         Flesh         = -0.25
modifierForDamageOnArmor Damage.Gas         ClonedFlesh   = -0.5
modifierForDamageOnArmor Damage.Gas         Infested      =  0.5
modifierForDamageOnArmor Damage.Magnetic    Shield        =  0.75
modifierForDamageOnArmor Damage.Magnetic    ProtoShield   =  0.75
modifierForDamageOnArmor Damage.Magnetic    Alloy         = -0.5
modifierForDamageOnArmor Damage.Radiation   Fossilized    = -0.75
modifierForDamageOnArmor Damage.Radiation   InfestedFlesh =  0.5
modifierForDamageOnArmor Damage.Radiation   Sinew         =  0.5
modifierForDamageOnArmor Damage.Radiation   Machinery     = -0.25
modifierForDamageOnArmor Damage.Radiation   Robotic       =  0.25
modifierForDamageOnArmor Damage.Radiation   Shield        = -0.25
modifierForDamageOnArmor Damage.Radiation   Alloy         =  0.75
modifierForDamageOnArmor Damage.Viral       Flesh         =  0.5
modifierForDamageOnArmor Damage.Viral       ClonedFlesh   =  0.75
modifierForDamageOnArmor _                  _             =  0

-- |enemyArmor gives the armor value for any given enemy with the base armor a,
-- base level b and current level l.
enemyArmor :: Int -> Int -> Int -> Float
enemyArmor a b l = fromIntegral a * (1 + ((fromIntegral (l - b) ** 1.75) / 200) :: Float)

-- |damageModifier is the factor for armor type t given type of damage d versus t given 
-- an armor value a. Health-type damages have a reduction of 0 making them constant.
damageModifier :: Damage.Type -> Float -> Armor -> Float
damageModifier d a t = (1 + modifierForDamageOnArmor d t) * 300 / (300 + a * (1 - modifierForDamageOnArmor d t))

-- |damageOnEnemy calculates the damage applied on a specific enemy given a level l and a
-- list of damages d.
damageOnEnemy :: Enemy -> Int -> [Damage.Damage] -> [Damage.Damage]
damageOnEnemy (Enemy a b) l d = [Damage.Damage (n * product [damageModifier t (enemyArmor v b l) at | ArmorValue at v <- a]) t | Damage.Damage n t <- d]


