module Main where

import System.IO (hFlush, stdout)
import Data.Char (toLower)
import Data.List (intercalate, nub)
import Data.Maybe (isJust)

-- ==================== DATA TYPES ====================

data Direction = North | South | East | West
    deriving (Eq, Show, Read)

data Item = Potion | Sword | Sigil | Key
    deriving (Eq, Show, Read)

itemName :: Item -> String
itemName Potion = "potion"
itemName Sword = "sword"
itemName Sigil = "sigil"
itemName Key = "key"

parseItem :: String -> Maybe Item
parseItem s = case map toLower s of
    "potion" -> Just Potion
    "sword"  -> Just Sword
    "sigil"  -> Just Sigil
    "key"    -> Just Key
    _        -> Nothing

data Location = ArcaneSanctum | WhisperingWoods | InfernalForge | ShadowLabyrinth | VoidThrone
    deriving (Eq, Show, Read)

locationName :: Location -> String
locationName ArcaneSanctum   = "Arcane Sanctum"
locationName WhisperingWoods = "Whispering Woods"
locationName InfernalForge   = "Infernal Forge"
locationName ShadowLabyrinth = "Shadow Labyrinth"
locationName VoidThrone      = "Void Throne"

data Enemy = Enemy
    { enemyName   :: String
    , enemyHealth :: Int
    , enemyDamage :: Int
    } deriving (Eq, Show)

data GameState = GameState
    { playerName     :: String
    , currentLocation :: Location
    , health         :: Int
    , maxHealth      :: Int
    , chaosPoints    :: Int
    , inventory      :: [Item]
    , availableItems :: [(Location, Item)]
    , defeated       :: [Location]
    , visited        :: [Location]
    , currentEnemy   :: Maybe Enemy
    , gameOver       :: Bool
    , won            :: Bool
    } deriving (Eq, Show)

-- ==================== INITIAL STATE ====================

initialState :: GameState
initialState = GameState
    { playerName      = "Mathias Mindblade"
    , currentLocation = ArcaneSanctum
    , health          = 100
    , maxHealth       = 100
    , chaosPoints     = 15
    , inventory       = []
    , availableItems  =
        [ (ArcaneSanctum,   Potion)
        , (WhisperingWoods, Sword)
        , (InfernalForge,   Sigil)
        , (ShadowLabyrinth, Key)
        , (VoidThrone,      Sigil)
        ]
    , defeated        = []
    , visited         = [ArcaneSanctum]
    , currentEnemy    = Nothing
    , gameOver        = False
    , won             = False
    }

-- ==================== WORLD LOGIC ====================

exitsFrom :: Location -> [(Direction, Location)]
exitsFrom ArcaneSanctum   = [(North, WhisperingWoods)]
exitsFrom WhisperingWoods = [(South, ArcaneSanctum), (East, InfernalForge)]
exitsFrom InfernalForge   = [(West, WhisperingWoods), (North, ShadowLabyrinth)]
exitsFrom ShadowLabyrinth = [(South, InfernalForge), (East, VoidThrone)]
exitsFrom VoidThrone      = [(West, ShadowLabyrinth)]

enemyAt :: Location -> Maybe Enemy
enemyAt ShadowLabyrinth = Just (Enemy "Gloom Wraith" 55 14)
enemyAt VoidThrone      = Just (Enemy "Void Sovereign" 130 25)
enemyAt _               = Nothing

-- Special navigation rule: need Key for Void Throne
canMove :: GameState -> Direction -> Maybe Location
canMove state dir =
    case lookup dir (exitsFrom (currentLocation state)) of
        Just VoidThrone | Key `notElem` inventory state -> Nothing
        res -> res

move :: Direction -> GameState -> GameState
move dir state =
    case canMove state dir of
        Just newLoc ->
            let pointGain  = if newLoc `elem` visited state then 4 else 15
                newVisited = nub (newLoc : visited state)
                newPoints  = chaosPoints state + pointGain
                isDefeated = newLoc `elem` defeated state
                newEnemy   = if isDefeated then Nothing else enemyAt newLoc
            in state
                { currentLocation = newLoc
                , chaosPoints     = newPoints
                , visited         = newVisited
                , currentEnemy    = newEnemy
                }
        Nothing -> state

-- Remove one occurrence of an item from inventory
removeOne :: Eq a => a -> [a] -> [a]
removeOne _ [] = []
removeOne x (y:ys)
    | x == y    = ys
    | otherwise = y : removeOne x ys

takeItem :: Item -> GameState -> GameState
takeItem it state =
    let loc     = currentLocation state
        pair    = (loc, it)
        newAvail = filter (/= pair) (availableItems state)
        newInv   = it : inventory state
        newPoints = chaosPoints state + 6
    in state
        { inventory      = newInv
        , availableItems = newAvail
        , chaosPoints    = newPoints
        }

useItem :: String -> GameState -> GameState
useItem itemStr state =
    case parseItem itemStr of
        Just Potion ->
            if Potion `elem` inventory state then
                let newH   = min (maxHealth state) (health state + 40)
                    newInv = removeOne Potion (inventory state)
                in state { health = newH, inventory = newInv }
            else state
        _ -> state

-- Combat: one round of attack
attack :: GameState -> GameState
attack state =
    case currentEnemy state of
        Nothing -> state
        Just enemy ->
            let swordBonus = if Sword `elem` inventory state then 12 else 0
                chaosBonus = chaosPoints state `div` 7
                pDmg       = 22 + swordBonus + chaosBonus
                newEHealth = enemyHealth enemy - pDmg
            in if newEHealth <= 0
                -- Enemy defeated
                then
                    let loc       = currentLocation state
                        newDef    = nub (loc : defeated state)
                        pointGain = if loc == VoidThrone then 120 else 30
                        isBossWin = loc == VoidThrone
                    in state
                        { currentEnemy = Nothing
                        , defeated     = newDef
                        , chaosPoints  = chaosPoints state + pointGain
                        , won          = isBossWin
                        , gameOver     = isBossWin
                        }
                else
                    -- Enemy survives and counterattacks
                    let stateAfterHit = state
                            { currentEnemy = Just enemy { enemyHealth = newEHealth } }
                        newPHealth = health stateAfterHit - enemyDamage enemy
                        playerDead = newPHealth <= 0
                    in stateAfterHit
                        { health   = max 0 newPHealth
                        , gameOver = playerDead
                        , won      = False
                        }

flee :: GameState -> GameState
flee state =
    case currentEnemy state of
        Just _ -> state
            { currentEnemy = Nothing
            , chaosPoints  = max 0 (chaosPoints state - 8)
            }
        Nothing -> state

-- ==================== DISPLAY ====================

describe :: GameState -> String
describe state =
    let loc = currentLocation state
        enemyStr = case currentEnemy state of
            Just e -> "\n[ENEMY] " ++ enemyName e ++ " (HP: " ++ show (enemyHealth e) ++ ") blocks your path!"
            Nothing -> ""
    in case loc of
        ArcaneSanctum ->
            "The Arcane Sanctum hums with latent power. Glowing runes pulse like heartbeats on black stone. This is your sanctum, Chaos Mage." ++ enemyStr
        WhisperingWoods ->
            "Ancient trees twist and murmur forgotten incantations. A blade glints among the roots, calling to your hand." ++ enemyStr
        InfernalForge ->
            "Searing heat and floating molten runes. The air smells of brimstone and creation. Sigils of power drift like embers." ++ enemyStr
        ShadowLabyrinth ->
            "Darkness writhes. Every shadow feels alive and hungry. Something malevolent waits in the gloom." ++ enemyStr
        VoidThrone ->
            "A vast obsidian throne room. The fabric of reality frays at the edges. Upon the throne sits the Void Sovereign — the final test of your ambition." ++ enemyStr

hud :: GameState -> String
hud state =
    let border = replicate 52 '='
        locName = locationName (currentLocation state)
        hpLine  = show (health state) ++ "/" ++ show (maxHealth state)
        cpLine  = show (chaosPoints state)
        invList = if null (inventory state)
                  then "Empty"
                  else intercalate ", " (map itemName (inventory state))
        enemyLine = case currentEnemy state of
            Just e -> "\n>>> FOE PRESENT: " ++ enemyName e ++ " [HP: " ++ show (enemyHealth e) ++ "] <<<"
            Nothing -> ""
        endMsg = if gameOver state
                 then if won state
                      then "\n*** VICTORY - THE CHAOS CODEX IS YOURS ***"
                      else "\n*** YOU HAVE FALLEN TO THE VOID ***"
                 else ""
    in unlines
        [ border
        , "          MATHIAS MINDBLADE — CHAOS RPG"
        , border
        , " Location      : " ++ locName
        , " Health        : " ++ hpLine ++ "     Chaos Points : " ++ cpLine
        , " Inventory     : " ++ invList
        , enemyLine
        , endMsg
        , border
        ]

helpText :: String
helpText = unlines
    [ "=== COMMANDS ==="
    , "  look / l                 Examine your surroundings"
    , "  move <dir> / go / walk   Move (north/n, south/s, east/e, west/w)"
    , "  take / get <item>        Pick up an item (potion, sword, sigil, key)"
    , "  use / drink <item>       Use a potion to heal"
    , "  attack / fight / strike  Engage current enemy (one combat round)"
    , "  flee / run               Retreat from battle (small Chaos Point cost)"
    , "  inventory / inv / i      Show your items"
    , "  stats / status           Show detailed numbers"
    , "  help                     Show this help"
    , "  quit / exit              Abandon the quest"
    , ""
    , "TIP: Explore thoroughly, find the Key to reach the final throne,"
    , "     defeat the Void Sovereign to WIN."
    ]

-- ==================== COMMAND PROCESSING ====================

parseDirection :: String -> Maybe Direction
parseDirection "north" = Just North
parseDirection "n"     = Just North
parseDirection "south" = Just South
parseDirection "s"     = Just South
parseDirection "east"  = Just East
parseDirection "e"     = Just East
parseDirection "west"  = Just West
parseDirection "w"     = Just West
parseDirection _       = Nothing

processCommand :: [String] -> GameState -> IO GameState
processCommand [] state = return state
processCommand (cmd:args) state
    | cmd == "quit" || cmd == "exit" = do
        putStrLn "You step back from the precipice. The Chaos Mage fades into legend..."
        return state { gameOver = True, won = False }

    | cmd == "help" = do
        putStrLn helpText
        return state

    | cmd == "look" || cmd == "l" = do
        putStrLn (describe state)
        return state

    | cmd == "inventory" || cmd == "inv" || cmd == "i" = do
        let invStr = if null (inventory state)
                     then "Your inventory is empty."
                     else "You carry: " ++ intercalate ", " (map itemName (inventory state))
        putStrLn invStr
        return state

    | cmd == "stats" || cmd == "status" = do
        putStrLn $ "Health: " ++ show (health state) ++ "/" ++ show (maxHealth state)
        putStrLn $ "Chaos Points: " ++ show (chaosPoints state)
        putStrLn $ "Location: " ++ locationName (currentLocation state)
        putStrLn $ "Visited locations: " ++ show (length (visited state)) ++ " / 5"
        return state

    | cmd == "move" || cmd == "go" || cmd == "walk" = case args of
        (dirStr:_) ->
            case parseDirection (map toLower dirStr) of
                Just d -> do
                    let newState = move d state
                    if currentLocation newState /= currentLocation state
                        then do
                            putStrLn $ "You stride " ++ show d ++ " into the " ++ locationName (currentLocation newState) ++ "."
                            putStrLn (describe newState)
                            return newState
                        else do
                            putStrLn "You cannot go that way."
                            return state
                Nothing -> do
                    putStrLn "Unknown direction. Use: north, south, east, west (or n/s/e/w)"
                    return state
        _ -> do
            putStrLn "Move where? Example: move north"
            return state

    | cmd == "take" || cmd == "get" || cmd == "pickup" = case args of
        (itemStr:_) ->
            case parseItem itemStr of
                Just it ->
                    let loc = currentLocation state
                    in if any (\(l, i) -> l == loc && i == it) (availableItems state)
                       then do
                           let newState = takeItem it state
                           putStrLn $ "You take the " ++ itemName it ++ ". Chaos stirs within you."
                           return newState
                       else do
                           putStrLn "There is nothing like that here to take."
                           return state
                Nothing -> do
                    putStrLn "You don't know how to take that."
                    return state
        _ -> do
            putStrLn "Take what? (potion / sword / sigil / key)"
            return state

    | cmd == "use" || cmd == "drink" = case args of
        (itemStr:_) -> do
            let newState = useItem itemStr state
            if health newState > health state
                then do
                    putStrLn "The potion's warmth spreads through your veins. Your wounds close."
                    return newState
                else do
                    putStrLn "You have no potion to use, or it has no effect right now."
                    return state
        _ -> do
            putStrLn "Use what?"
            return state

    | cmd == "attack" || cmd == "fight" || cmd == "strike" = do
        if isJust (currentEnemy state)
            then do
                let newState = attack state
                putStrLn "You unleash a surge of chaotic will!"
                case currentEnemy newState of
                    Just e ->
                        putStrLn $ "The " ++ enemyName e ++ " reels! It has " ++ show (enemyHealth e) ++ " HP remaining and counterattacks!"
                    Nothing ->
                        if won newState
                            then putStrLn "The Void Sovereign dissolves into screaming void! The throne is yours!"
                            else putStrLn "Your enemy collapses into dust. Raw chaos floods your being."
                if health newState < health state && health newState > 0
                    then putStrLn $ "You suffer a vicious blow! Your health is now " ++ show (health newState) ++ "."
                    else if health newState <= 0
                        then putStrLn "Darkness takes you..."
                        else return ()
                return newState
            else do
                putStrLn "There is no enemy here to fight."
                return state

    | cmd == "flee" || cmd == "run" = do
        if isJust (currentEnemy state)
            then do
                putStrLn "You break away from combat, the shadows nipping at your heels..."
                return (flee state)
            else do
                putStrLn "There is nothing to flee from."
                return state

    | otherwise = do
        putStrLn "Command not recognized. Type 'help' to see available commands."
        return state

-- ==================== GAME LOOP ====================

gameLoop :: GameState -> IO ()
gameLoop state = do
    putStrLn (hud state)
    if gameOver state
        then do
            if won state
                then putStrLn "\n╔════════════════════════════════════════════════════════════╗"
                     >> putStrLn   "║  VICTORY! Mathias Mindblade has claimed the Void Throne.   ║"
                     >> putStrLn   "║  The Chaos Codex bends to your will. You are the new Lord  ║"
                     >> putStrLn   "║  of this realm and all threads that weave through it.      ║"
                     >> putStrLn   "╚════════════════════════════════════════════════════════════╝"
                else putStrLn "\nThe void has claimed another soul. Mathias Mindblade falls..."
                     >> putStrLn   "Yet in the multiverse of chaos, every ending is merely a new beginning..."
            putStrLn "\nThank you for playing Mathias Mindblade: Chaos RPG"
        else do
            putStr "> "
            hFlush stdout
            input <- getLine
            let cmdWords = words (map toLower input)
            newState <- processCommand cmdWords state
            gameLoop newState

-- ==================== MAIN ====================

main :: IO ()
main = do
    putStrLn "╔══════════════════════════════════════════════════════════════════╗"
    putStrLn "║           MATHIAS MINDBLADE: CHAOS RPG                           ║"
    putStrLn "║     A text-based adventure of magick, combat, and ambition       ║"
    putStrLn "╚══════════════════════════════════════════════════════════════════╝"
    putStrLn "\nYou are Mathias Mindblade, Chaos Mage and seeker of the Codex."
    putStrLn "The Void Throne awaits. Will you claim it... or be consumed?"
    putStrLn "\nType 'help' at any time for commands.\n"
    gameLoop initialState
