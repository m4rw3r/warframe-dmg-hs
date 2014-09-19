{-# LANGUAGE RecursiveDo #-}

import Control.Monad (void)
import Data.List
import Data.Maybe
import Data.Monoid
import Math.Combinat.Sets (choose)
import Text.Printf
import qualified Numeric.Probability.Distribution as Dist

import Damage
import Mod
import Weapon
import Utils
import qualified Rifles
import qualified Pistols
import qualified Shotguns

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (delete)

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = void $ mdo
    return window # set title "Warframe Mod Optimizer"
    
    selType <- UI.select # set UI.name "weapon-type-select"
                     # set UI.id_ "weapon-type-select" #+ [
                         UI.option # set UI.selected True #+ [string "Rifle"], 
                         UI.option #+ [string "Shotgun"],
                         UI.option #+ [string "Pistol"]
                         ]
    selWeapon <- UI.select # set UI.name "weapon-select"
                    # set UI.id_ "weapon-select" #+ (weaponOption <$> (weaponsOfType Rifle))
    chosenType <- UI.p
    
    getBody window #+ [element selType, element chosenType, element selWeapon]
    
    -- events
    let eType = UI.selectionChange selType
    
    -- behaviours
    bWeapon <- fmap (selToWeaponType <$>) $ stepper (Just 0) eType
    let cWeapons = (weaponsOfType) <$> bWeapon
    
    element chosenType # sink UI.text (show <$> bWeapon)
    onChanges cWeapons $ \w -> do
        let elems = (weaponOption <$> w)
        element selWeapon # set UI.children [] #+ elems

weaponOption :: Weapon -> UI Element
weaponOption w = UI.option # set UI.id_ (name w) #+ [string $ name w]

data WeaponType = Rifle | Shotgun | Pistol
    deriving (Show)

selToWeaponType :: Maybe Int -> WeaponType
selToWeaponType (Just 0) = Rifle
selToWeaponType (Just 1) = Shotgun
selToWeaponType (Just 2) = Pistol
selToWeaponType _ = Rifle

weaponsOfType :: WeaponType -> [Weapon]
weaponsOfType Rifle = [Rifles.lanka, Rifles.dread, Rifles.vulkar]
weaponsOfType Shotgun = [Shotguns.drakgoonUncharged, Shotguns.drakgoonCharged]
weaponsOfType Pistol  = [Pistols.despair]
