import Prelude hiding (Left, Right) -- FIXME
import Control.Monad.State
import Control.Monad.Identity

-- import Instruction
type MarkerNumber = Int -- 0..5
type InvChance = Int -- 1.. (1 / 1 == 100%, 1 / 2 == 50%, 1 / 3 == 33%)

data SenseDir
    = Here
    | Ahead
    | LeftAhead
    | RightAhead
    deriving Show

data LeftOrRight
    = Left
    | Right
    deriving Show

data Condition
    = Friend
    | Foe
    | FriendWithFood
    | FoeWithFood
    | Food
    | Rock
    | Marker MarkerNumber
    | FoeMarker
    | Home
    | FoeHome
    deriving Show

data Instruction
    = Sense SenseDir Instruction Instruction Condition
    | Mark MarkerNumber Instruction
    | Unmark MarkerNumber Instruction
    | PickUp Instruction Instruction
    | Drop Instruction
    | Turn LeftOrRight Instruction
    | Move Instruction Instruction
    | Flip InvChance Instruction Instruction
    | Goto Int
    deriving Show


data DSLState = DSLState Int [(Int, Instruction)] deriving Show

typeGoto :: State DSLState Instruction
typeGoto = do (DSLState u is) <- get
              put (DSLState (u+1) is)
              return (Goto u)

defineAs :: Instruction -> Instruction -> State DSLState ()
defineAs (Goto d) i = do (DSLState u is) <- get
                         put (DSLState u ((d,i):is))


program :: State DSLState ()
program = do
    -- Definitions
    start      <- typeGoto
    pickupFood <- typeGoto
    search     <- typeGoto
    goHome     <- typeGoto
    notHome    <- typeGoto
    foundHome  <- typeGoto

    -- Bodies
    start      `defineAs` Sense Ahead pickupFood search Food
    pickupFood `defineAs` Move (PickUp goHome start) start
    search     `defineAs` (Flip 3 (Turn Left start) (Flip 2 (Turn Right start) (Move start search)))
    goHome     `defineAs` Sense Ahead foundHome notHome Home
    notHome    `defineAs` Flip 3 (Turn Left goHome) (Flip 2 (Turn Right goHome) (Move goHome notHome))
    foundHome  `defineAs` Move (Drop start) goHome

buildProgram :: State DSLState () -> [(Int, Instruction)]
buildProgram p = let DSLState _ is = snd $ (runState p) (DSLState 0 []) in is

-- buildProgram program