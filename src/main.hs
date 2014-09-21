{-# LANGUAGE RecursiveDo #-}

import Control.Monad (void, liftM)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Vector (fromList, empty)
import Math.Combinat.Sets (choose)
import Text.Printf
import qualified Numeric.Probability.Distribution as Dist
import qualified Data.ByteString.Char8 as BS
import System.IO (hPutStrLn, stderr)

import Damage
import Mod
import Weapon
import Utils
import qualified Rifles
import qualified Pistols
import qualified Shotguns

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (delete)
import Data.Aeson.Encode
import Data.Aeson.Types as JSON

tpConfig = Config
    { tpPort = Nothing
    , tpCustomHTML = Nothing
    , tpStatic = Just "wwwroot"
    , tpLog = \s -> BS.hPutStrLn stderr s
    }

main :: IO ()
main = startGUI tpConfig setup

setup :: Window -> UI ()
setup window = void $ mdo
    addScriptTag window "lib/jquery.flot.js"
    UI.addStyleSheet window "styles.css"
    
    return window # set title "Warframe Mod Optimizer"
    
    elSelType   <- UI.select # set UI.name "weapon-type-select"
                     # set UI.id_ "weapon-type-select" #+ [
                         UI.option #+ [string "Select type"],
                         UI.option #+ [string "Rifle"],
                         UI.option #+ [string "Shotgun"],
                         UI.option #+ [string "Pistol"]
                         ]
    elSelWeapon  <- UI.select # set UI.name "weapon-select"
                    # set UI.id_ "weapon-select"
    elSelMods    <- UI.div
    elChosenMods <- UI.ul
    elChosenWeapon <- UI.div
    elModdedWeapon <- UI.div
    dps            <- UI.p
    elChart        <- UI.div # set UI.id_ "dmg_probabilities" # set UI.height 400 # set UI.width 600
    
    getBody window #+ [grid [
            [
            UI.new #+ [string "Weapon type: ", element elSelType], UI.new #+ [string "Weapon: ", element elSelWeapon]
            ],
            [
            UI.new #+ [UI.new #+ [string "Mods: "], element elSelMods], element elChosenWeapon, element elChosenMods, element elModdedWeapon, UI.new #+ [element dps, element elChart]
            ]
            ]]
    
    -- events
    let eType    = UI.selectionChange elSelType
    let eWeapon  = UI.selectionChange elSelWeapon
    let eWeapObj = bSelToWeapon <@> eWeapon
    
    -- behaviours
    bWeaponType  <- fmap (selToWeaponType <$>) $ stepper Nothing eType
    bApplyMods   <- fmap (maybeApplyMods  <$>) $ stepper Nothing eWeapObj
    bWeapon      <- stepper Nothing eWeapObj
    
    let bWeaponList  = (fmap weaponsOfType) <$> bWeaponType
    let bModList     = modsOfType    <$> bWeaponType
    let bSelToWeapon = selToWeapon   <$> bWeaponList
    
    onChanges bWeapon $ \w -> do
        element elChosenWeapon # set UI.children [] #+ case w of
            Just w  -> [displayWeapon w]
            Nothing -> [UI.new]
    onChanges bWeaponType $ \w -> do
        (bChecked, el) <- mkModList (modsOfType w)
        element elSelMods # set UI.children [] #+ [element el]
        
        let bMods         = fmap modData <$> bChecked
        let bModdedWeapon = bApplyMods <*> bMods
        let bDPS          = fmap damagePerSecond <$> bModdedWeapon
        let bModList      = fmap modListItem <$> bChecked
        
        element dps # sink UI.text (show <$> bDPS)
        onChanges bModdedWeapon $ \w -> do
            runFunction $ invokeGraph "#dmg_probabilities" (case w of
                Just w  -> (toPairs (damageProbabilities w))
                Nothing -> JSON.Array Data.Vector.empty)
            element elModdedWeapon # set UI.children [] #+ case w of
                Just w  -> [displayWeapon w]
                Nothing -> [UI.new]
        onChanges bModList $ \m -> do
            element elChosenMods # set UI.children [] #+ m
    
    onChanges bWeaponList $ \w -> do
        element elSelWeapon # set UI.children [] #+ ([UI.option #+ [string "Select Weapon"]] ++ (weaponOption <$> (fromMaybe [] w)))

weaponOption :: Weapon -> UI Element
weaponOption w = UI.option # set UI.id_ (name w) #+ [string $ name w]

modListItem :: ModChecked -> UI Element
modListItem m = UI.li #+ [UI.p #+ [string $ modName (modData m)],
                          UI.p #+ [string $ intercalate ", " (fmap show (modValues (modData m)))]]

displayWeapon :: Weapon -> UI Element
displayWeapon w = grid ([
                    [UI.string (name w)],
                    [UI.new # set html "&nbsp;"],
                    [UI.string "Accuracy",            UI.string $ printf "%.0f" (100 * accuracy w)],
                    [UI.string "Total ammo",          UI.string $ printf "%d" (capacity w)],
                    [UI.string "Critical chance",     UI.string $ printf "%.1f%%" (100 * critChance w)],
                    [UI.string "Critical multiplier", UI.string $ printf "%.2f" (critMultiplier w)],
                    [UI.string "Fire rate",           UI.string $ printf "%.2f" (fireRate w)],
                    [UI.string "Magazine size",       UI.string $ printf "%d" (magazine w)],
                    [UI.string "Multishot",           UI.string $ printf "%.0f%%" (100 * multishot w)],
                    [UI.string "Reload time",         UI.string $ printf "%.2f" (reload w)],
                    [UI.string "Status Chance",       UI.string $ printf "%.1f%%" (100 * status w)],
                    [UI.new # set html "&nbsp;"]
                    ] ++ (fmap displayDamage (damage w)))

toPairs :: Dist.T Float [Damage] -> JSON.Value
toPairs d = JSON.Array (fmap (\(x, y) -> JSON.Array (fromList [JSON.toJSON $ sumDamage x, JSON.toJSON y])) (fromList $ (sortBy (\(x1, _) (x2, _) -> compare x1 x2) (filter (\(x, y) -> y > 0) $ Dist.decons d))))

invokeGraph :: String -> JSON.Value -> JSFunction ()
invokeGraph = ffi "$.plot(%1, [{data: %2, bars: { show: true, lineWidth: 5, fill: true}}], {yaxis: {min: 0.0}, xaxis: {min: 0.0, }});"

displayDamage :: Damage -> [UI Element]
displayDamage (Damage d t) = [UI.string $ show t, UI.string $ printf "%.1f" d]

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

selToWeaponType :: Maybe Int -> Maybe WeaponType
selToWeaponType (Just 1) = Just Rifle
selToWeaponType (Just 2) = Just Shotgun
selToWeaponType (Just 3) = Just Pistol
selToWeaponType _        = Nothing

selToWeapon :: Maybe [Weapon] -> Maybe Int -> Maybe Weapon
selToWeapon (Just []) _       = Nothing
selToWeapon (Just w) (Just i) | i > 0 = Just $ w !! (i - 1)
selToWeapon _ _ = Nothing

modsOfType :: Maybe WeaponType -> [Mod]
modsOfType (Just Rifle)   = Rifles.mods
modsOfType (Just Shotgun) = Shotguns.mods
modsOfType (Just Pistol)  = Pistols.mods
modsOfType Nothing = []

weaponsOfType :: WeaponType -> [Weapon]
weaponsOfType Rifle   = [Rifles.lanka, Rifles.dread, Rifles.vulkar]
weaponsOfType Shotgun = [Shotguns.drakgoonUncharged, Shotguns.drakgoonCharged]
weaponsOfType Pistol  = [Pistols.despair]
weaponsOfType None    = []

addScriptTag :: UI.Window -> String -> UI ()
addScriptTag w filename = void $ do
    el <- UI.mkElement "script"
        # set UI.src ("/static/" ++ filename)
    getHead w #+ [element el]
