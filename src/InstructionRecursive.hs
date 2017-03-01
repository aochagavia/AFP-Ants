module InstructionRecursive (
    MarkerNumber,
    InvChance,
    SenseDir(..),
    LeftOrRight(..),
    Condition(..),
    Instruction(..)
    ) where

import Prelude hiding (Left, Right)

type AntState = Int -- 0..9999
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
    deriving Show

{-
The default program that is implemented recursively
defaultProgram' :: [Instruction]
defaultProgram' = [ Sense Ahead 1 3 Food -- state 0: [SEARCH] is there food in front of me?
                  , Move 2 0 -- state 1: YES: move onto food (return to state 0 on failure)
                  , PickUp 8 0 -- state 2: pick up food and jump to state 8 (or 0 on failure)
                  , Flip 3 4 5 -- state 3: NO: choose whether to...
                  , Turn Left 0 -- state 4: turn left and return to state 0
                  , Flip 2 6 7  -- state 5: ...or...
                  , Turn Right 0 -- state 6: turn right and return to state 0
                  , Move 0 3 -- state 7: ...or move forward and return to state 0 (or 3 on failure)
                  , Sense Ahead 9 11 Home -- state 8: [GO HOME] is the cell in front of me my anthill?
                  , Move 10 8 -- state 9: YES: move onto anthill
                  , Drop 0 -- state 10: drop food and return to searching
                  , Flip 3 12 13 -- state 11: NO: choose whether to...
                  , Turn Left 8 -- state 12: turn left and return to state 8
                  , Flip 2 14 15 -- state 13: ...or...
                  , Turn Right 8 -- state 14: turn right and return to state 8
                  , Move 8 11 -- state 15: ...or move forward and return to state 8
                  ]
-}

start :: Instruction
start = Sense Ahead pickupFood search Food

pickupFood :: Instruction
pickupFood = Move (PickUp goHome start) start

search :: Instruction
search = Flip 3 (Turn Left start) (Flip 2 (Turn Right start) (Move start search))

goHome :: Instruction
goHome = Sense Ahead foundHome notHome Home

notHome :: Instruction
notHome = Flip 3 (Turn Left goHome) (Flip 2 (Turn Right goHome) (Move goHome notHome))

foundHome :: Instruction
foundHome = Move (Drop start) goHome

-- Idea: use a uniqSupply like Monad to get unique identifiers for each definition.
-- Then define "defineAs" somehow.
{-
program = do
    -- Definitions
    start      <- typeGoto
    pickupFood <- typeGoto
    search     <- typeGoto
    goHome     <- typeGoto
    foundHome  <- typeGoto

    -- Bodies
    start      `defineAs` Sense Ahead pickupFood search Food
    pickupFood `defineAs` Move (PickUp goHome start) start
    search     `defineAs` Flip 3 (Turn Left start) (Flip 2 (Turn Right start) (Move start search))
    goHome     `defineAs` Sense Ahead foundHome (Flip 3 (Turn Left goHome) (Flip 2 (Turn Right goHome) (Move goHome))) Home
    foundHome  `defineAs` Move (Drop start) goHome
-}