module Language.Instruction (
    AntState,
    MarkerNumber,
    InvChance,
    SenseDir(..),
    LeftOrRight(..),
    Condition(..),
    Instruction(..),
    showInstruction,
    ) where

import Data.List (intercalate)

type AntState = Int -- 0..9999
type MarkerNumber = Int -- 0..5
type InvChance = Int -- 1.. (1 / 1 == 100%, 1 / 2 == 50%, 1 / 3 == 33%)

data SenseDir
    = Here
    | Ahead
    | LeftAhead
    | RightAhead
    deriving (Show, Eq)

data LeftOrRight
    = Left
    | Right
    deriving (Show, Eq)

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
    deriving (Eq, Show)

data Instruction
    = Sense SenseDir AntState AntState Condition
    | Mark MarkerNumber AntState
    | Unmark MarkerNumber AntState
    | PickUp AntState AntState
    | Drop AntState
    | Turn LeftOrRight AntState
    | Move AntState AntState
    | Flip InvChance AntState AntState
    deriving (Eq, Show)

showInstruction :: Instruction -> String
showInstruction (Sense dir a1 a2 c) = unwords ["Sense", show dir, show a1, show a2, show c]
showInstruction x = show x
