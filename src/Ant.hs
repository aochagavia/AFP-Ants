module Ant (
    fragmentProgram,
    programReinier
) where

import Prelude hiding (Either(..))

import Control.Monad.State
import Language.Compiler (compileProgram)
import Language.Fragment
import Language.Function
import Language.Instruction hiding (Instruction(..))
import Language.Optimizer

import qualified Language.Instruction as I
import qualified Prelude as P

programReinier :: ProgramBuilder ()
programReinier = do
    ----- Definitions -----
    -- number of ants = 91
    -- number of circles = 6 (0..5)
    start           <- declare
    error           <- declare -- non reachable state

    --- Init ---
    selectCircle0   <- declare
    selectCircle1   <- declare
    selectCircle2   <- declare
    selectCircle3   <- declare
    selectCircle4   <- declare

    -- guard
    cornerScan      <- declare
    foodPlace       <- declare
    guardLeft       <- declare
    forceMove       <- declare
    scanGuard       <- declare
    guardScan       <- declare
    guardRight      <- declare

    cornerScan1     <- declare
    changeGuard     <- declare
    guardBehind     <- declare
    change          <- declare

    onFoodPlace     <- declare
    changingGuard   <- declare

    --- Collector ---
    collectorFind   <- declare
    otherFind       <- declare
    foodFind        <- declare
    foodLost        <- declare
    blockedFind     <- declare
    collectorFound  <- declare
    blockedFound    <- declare

    -- exit without food
    exit5           <- declare
    exit5Move       <- declare
    exit4           <- declare
    exit4Move       <- declare
    exit3           <- declare
    exit3Move       <- declare
    exit2           <- declare
    exit2Move       <- declare
    exit1           <- declare
    exit1Move       <- declare
    exit0           <- declare
    exit0Move       <- declare

    ----- Bodies -----
    start           `defineAs` selectCircle0
    error           `defineAs` Drop error -- non reachable state -- infinite loop of nops -- ants that stand still

    --- init phase
    selectCircle0   `execute`  turnCond Left notHome    (Mark 0 cornerScan)     selectCircle1
    selectCircle1   `execute`  turnCond Left (marker 0) (Mark 1 cornerScan1)    selectCircle2
    selectCircle2   `execute`  turnCond Left (marker 1) (Mark 2 exit2)          selectCircle3
    selectCircle3   `execute`  turnCond Left (marker 2) (Mark 3 exit3)          selectCircle4
    selectCircle4   `execute`  turnCond Left (marker 3) (Mark 4 exit4)          (Mark 5 exit5)
    {- Home looks like this (numbers are marks)
    . . . . . . . . . . . . .
     . . . 0 0 0 0 0 0 . . .
    . . . 0 1 1 1 1 1 0 . . .
     . . 0 1 2 2 2 2 1 0 . .
    . . 0 1 2 3 3 3 2 1 0 . .
     . 0 1 2 3 4 4 3 2 1 0 .
    . 0 1 2 3 4 5 4 3 2 1 0 .
     . 0 1 2 3 4 4 3 2 1 0 .
    . . 0 1 2 3 3 3 2 1 0 . .
     . . 0 1 2 2 2 2 1 0 . .
    . . . 0 1 1 1 1 1 0 . . .
     . . . 0 0 0 0 0 0 . . .
    . . . . . . . . . . . . .
    -}

    --- guards ---
    cornerScan      `defineAs` Sense RightAhead (Turn Right cornerScan) foodPlace notHome
    foodPlace       `defineAs` Turn Left (Sense LeftAhead (Mark 5 guardLeft) scanGuard notHome)
    guardLeft       `defineAs` Move (Mark 1 (Turn Left (Turn Left forceMove))) guardLeft
    forceMove       `defineAs` Move error forceMove
    scanGuard       `execute`  turnCond Left (marker 5) (Mark 4 error) guardScan
    guardScan       `execute`  turnCond Left (marker 4) (Sense RightAhead (Turn Right guardRight) exit0 notHome) exit0
    guardRight      `defineAs` Move (Mark 1 (Turn Left forceMove)) guardRight

    --- guard that gets exchanged ---
    cornerScan1     `defineAs` Sense RightAhead (Turn Right cornerScan1) changeGuard (marker 0)
    changeGuard     `defineAs` Turn Left (Sense Ahead (Mark 5 guardBehind) exit1 (And (marker 0) (marker 5)))
    guardBehind     `defineAs` Sense Ahead change guardBehind friend
    change          `defineAs` Turn Left (Turn Left (Move (Turn Right (Move exit0 exit1)) exit1))

    --- drop food and exchange the guard ---
    onFoodPlace     `defineAs` Drop (Move changingGuard onFoodPlace)
    changingGuard   `execute`  turn Back guardBehind

    --- Collector ---
    -- search
    collectorFind   `defineAs` Sense Ahead foodFind otherFind food -- turn Back enter0
    otherFind       `defineAs` Sense Ahead blockedFind (Move collectorFind blockedFind) home
    foodFind        `defineAs` Move (PickUp collectorFound foodLost) blockedFind
    foodLost        `execute`  turnCond Left food foodFind blockedFind
    blockedFind     `execute`  randomDirection collectorFind
    -- return
    collectorFound  `defineAs` Sense Ahead (Move onFoodPlace collectorFound) (Move collectorFound blockedFound) (And home (And (marker 5) (marker 0)))
    blockedFound    `execute`  randomDirection collectorFound

    -- Exit home after food dropped or after start
    exit5           `defineAs` Sense Ahead exit5Move (Turn Right exit5) (And (marker 4) noAnts)
    exit5Move       `defineAs` Move exit4 exit5 -- Wait for a free location in circle 4
    exit4           `defineAs` Sense Ahead exit4Move (Turn Right exit4) (And (marker 3) noAnts)
    exit4Move       `defineAs` Move exit3 exit4 -- Wait for a free location in circle 3
    exit3           `defineAs` Sense Ahead exit3Move (Turn Right exit3) (And (marker 2) noAnts)
    exit3Move       `defineAs` Move exit2 exit3 -- Wait for a free location in circle 2
    exit2           `defineAs` Sense Ahead exit2Move (Turn Right exit2) (And (marker 1) noAnts)
    exit2Move       `defineAs` Move exit1 exit2 -- Wait for a free location in circle 1
    exit1           `defineAs` Sense Ahead exit1Move (Turn Right exit1) (And (marker 0) noAnts)
    exit1Move       `defineAs` Move exit0 exit1 -- Wait for a free location in circle 0
    exit0           `defineAs` Sense Ahead exit0Move (Turn Right exit0) (And notHome noAnts)
    exit0Move       `defineAs` Move collectorFind exit0 -- Wait for a free location in outside of your home

    --- Entry point ---
    setEntryPoint start

marker :: MarkerNumber -> BoolExpr
marker = Cond . Marker

foe, foeWithFood, friend, friendWithFood, enemyAnts, friendlyAnts, ants, noAnts, food, foodOrAnts, foodNoAnts, home, notHome :: BoolExpr
foe             = Cond Foe
foeWithFood     = Cond FoeWithFood
friend          = Cond Friend
friendWithFood  = Cond FriendWithFood
enemyAnts       = Or foe foeWithFood
friendlyAnts    = Or friend friendWithFood
ants            = Or enemyAnts friendlyAnts
noAnts          = Not ants
food            = Cond Food
foodOrAnts      = And food ants
foodNoAnts      = And food noAnts
home            = Cond Home
notHome         = Not home

fragmentProgram :: [I.Instruction]
fragmentProgram = compileProgram programReinier
