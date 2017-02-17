module Instruction (
    AntState,
    MarkerNumber,
    InvChance,
    SenseDir(..),
    LeftOrRight(..),
    Condition(..),
    Instruction(..)
    ) where

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
    = Sense SenseDir AntState AntState Condition
    | Mark MarkerNumber AntState
    | Unmark MarkerNumber AntState
    | PickUp AntState AntState
    | Drop AntState
    | Turn LeftOrRight AntState
    | Move AntState AntState
    | Flip InvChance AntState AntState
    deriving Show