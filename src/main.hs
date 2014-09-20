{-# LANGUAGE RecursiveDo #-}

import Control.Monad (void, liftM)
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
    
    selType   <- UI.select # set UI.name "weapon-type-select"
                     # set UI.id_ "weapon-type-select" #+ [
                         UI.option #+ [string "Select type"],
                         UI.option #+ [string "Rifle"],
                         UI.option #+ [string "Shotgun"],
                         UI.option #+ [string "Pistol"]
                         ]
    selWeapon  <- UI.select # set UI.name "weapon-select"
                    # set UI.id_ "weapon-select"
    selMods    <- UI.div
    chosenMods <- UI.ul
    chosenType <- UI.p
    dps <- UI.p
    
    getBody window #+ [string "Weapon type: ",
        element selType,
        element chosenType,
        string "Weapon: ",
        element selWeapon,
        string "Mods: ",
        element selMods,
        element chosenMods,
        element dps]
    
    -- events
    let eType    = UI.selectionChange selType
    let eWeapon  = UI.selectionChange selWeapon
    let eWeapObj = bSelToWeapon <@> eWeapon
    
    -- behaviours
    bWeaponType  <- fmap (selToWeaponType <$>) $ stepper Nothing eType
    bApplyMods   <- fmap (maybeApplyMods  <$>) $ stepper Nothing eWeapObj
    
    let bWeaponList  = weaponsOfType <$> bWeaponType
    let bModList     = modsOfType    <$> bWeaponType
    let bSelToWeapon = selToWeapon   <$> bWeaponList
    
    element chosenType # sink UI.text (show <$> bWeaponType)
    onChanges bWeaponType $ \w -> do
        (bChecked, el) <- mkModList (modsOfType w)
        element selMods # set UI.children [] #+ [element el]
        
        let bMods         = fmap modData <$> bChecked
        let bModdedWeapon = bApplyMods <*> bMods
        let bDPS          = fmap damagePerSecond <$> bModdedWeapon
        
        element dps # sink UI.text (show <$> bDPS)
        onChanges bChecked $ \m -> do
            element chosenMods # set UI.children [] #+ (modListItem <$> m)
    
    onChanges bWeaponList $ \w -> do
        element selWeapon # set UI.children [] #+ (weaponOption <$> w)

weaponOption :: Weapon -> UI Element
weaponOption w = UI.option # set UI.id_ (name w) #+ [string $ name w]

modListItem :: ModChecked -> UI Element
modListItem m = UI.li #+ [UI.p #+ [string $ modName (modData m)],
                          UI.p #+ [string $ intercalate ", " (fmap show (modValues (modData m)))]]

maybeApplyMods :: Maybe Weapon -> ([Mod] -> Maybe Weapon)
maybeApplyMods (Just w) = (\m -> Just $ applyMods w m)
maybeApplyMods _        = (\_ -> Nothing)

data ModChecked = ModChecked {
    modData    :: Mod,
    modChecked :: Bool
    }
    deriving (Show, Eq)

accumModChecked :: [ModChecked] -> [ModChecked] -> [ModChecked]
accumModChecked a b = filter modChecked (l ++ a)
    where
        l = deleteFirstsBy (\x y -> modData x == modData y) b a

mkModList :: [Mod] -> UI (Behavior [ModChecked], Element)
mkModList m = do
    elWrapper <- UI.new
    list <- sequence $ fmap mkModCheckbox m
    let eMods   = unions (fmap fst list)
        elBoxes = fmap snd list
    bMods   <- accumB [] (fmap accumModChecked eMods)
    
    element elWrapper #+ fmap element elBoxes
    
    return (bMods, elWrapper)

mkModCheckbox :: Mod -> UI (Event ModChecked, Element)
mkModCheckbox m = do
    elCheckbox <- UI.input # set UI.type_ "checkbox"
    elLabel    <- UI.div #+ [mkElement "label" #+ [element elCheckbox, string (modName m)]]
    
    let eChecked = UI.checkedChange elCheckbox
        eMod     = (fmap (\x -> case x of
            True  -> ModChecked m True
            False -> ModChecked m False) eChecked)
   
    return (eMod, elLabel)

checkedModName :: Element -> UI String
checkedModName e = do
    v <- e # get UI.checked
    case v of
        True  -> e # get UI.value
        False -> return ""

data WeaponType = Rifle | Shotgun | Pistol | None
    deriving (Show)

selToWeaponType :: Maybe Int -> WeaponType
selToWeaponType (Just 1) = Rifle
selToWeaponType (Just 2) = Shotgun
selToWeaponType (Just 3) = Pistol
selToWeaponType _        = None

selToWeapon :: [Weapon] -> Maybe Int -> Maybe Weapon
selToWeapon [] _       = Nothing
selToWeapon w (Just i) = Just $ w !! i
selToWeapon _ Nothing  = Nothing

modsOfType :: WeaponType -> [Mod]
modsOfType Rifle   = Rifles.mods
modsOfType Shotgun = Shotguns.mods
modsOfType Pistol  = Pistols.mods
modsOfType None    = []

weaponsOfType :: WeaponType -> [Weapon]
weaponsOfType Rifle   = [Rifles.lanka, Rifles.dread, Rifles.vulkar]
weaponsOfType Shotgun = [Shotguns.drakgoonUncharged, Shotguns.drakgoonCharged]
weaponsOfType Pistol  = [Pistols.despair]
weaponsOfType None    = []
