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

    -- guardLeft
    foodPlace       <- declare
    guardLeft       <- declare
    forceMove       <- declare
    scanGuard       <- declare

    --- Collector ---
    collectorFind   <- declare
    otherFind       <- declare
    foodFind        <- declare
    foodLost        <- declare
    blockedFind     <- declare
    collectorFound  <- declare
    blockedFound    <- declare
    -- enter with food
    enter0          <- declare
    enter0Scan      <- declare
    enter0Move      <- declare
    enter1          <- declare
    enter1Scan      <- declare
    enter1Move      <- declare
    enter2          <- declare
    enter2Scan      <- declare
    enter2Move      <- declare
    enter3          <- declare
    enter3Sign      <- declare
    enter3Move      <- declare
    enter4          <- declare
    enter4Move      <- declare
    -- exit without food
    exit4           <- declare
    exit4Sign       <- declare
    exit4Move       <- declare
    exit3           <- declare
    exit3Move       <- declare
    exit2           <- declare
    exit2Move       <- declare
    exit1           <- declare
    exit1Move       <- declare
    exit0           <- declare
    exit0Move       <- declare

    --- Defender ---
    defender        <- declare
    defender4on3    <- declare
    scanEnemy       <- declare
    scanExit        <- declare
    scanEnter       <- declare
    patrolD         <- declare
    -- kill
    kill            <- declare
    -- exit
    exitMark        <- declare
    exitSpin        <- declare
    exitUnmark      <- declare
    -- enter
    enterMark       <- declare
    enterSpin       <- declare
    enterUnmark     <- declare

    --- Centre defender ---
    cdefender       <- declare
    foodCD          <- declare
    checkPickupCD   <- declare
    pickupCD        <- declare
    returnCD        <- declare
    patch5Blocked   <- declare

    ----- Bodies -----
    start           `defineAs` selectCircle0
    error           `defineAs` Drop error -- non reachable state -- infinite loop of nops

    --- init phase
    selectCircle0   `execute`  turnCond Left notHome    (Mark 0 foodPlace)  selectCircle1
    selectCircle1   `execute`  turnCond Left (marker 0) (Mark 1 exit1)      selectCircle2
    selectCircle2   `execute`  turnCond Left (marker 1) (Mark 2 exit2)      selectCircle3
    selectCircle3   `execute`  turnCond Left (marker 2) (Mark 3 exit3)      selectCircle4
    selectCircle4   `execute`  turnCond Left (marker 3) (Mark 4 defender)   (Mark 5 cdefender)
    {- Home looks like this (numbers are marks)
     . . 0 0 0 0 0 0 . .
    . . 0 1 1 1 1 1 0 . .
     . 0 1 2 2 2 2 1 0 .
    . 0 1 2 3 3 3 2 1 0 .
     0 1 2 3 4 4 3 2 1 0
    0 1 2 3 4 5 4 3 2 1 0
     0 1 2 3 4 4 3 2 1 0
    . 0 1 2 3 3 3 2 1 0 .
     . 0 1 2 2 2 2 1 0 .
    . . 0 1 1 1 1 1 0 . .
     . . 0 0 0 0 0 0 . .
    -}

    --- food place to guardLeft ---
    foodPlace       `defineAs` Turn Left (Sense LeftAhead (Mark 5 guardLeft) scanGuard notHome)
    guardLeft       `defineAs` Move (Mark 1 (Turn Left (Turn Left forceMove))) guardLeft
    forceMove       `defineAs` Move error forceMove
    scanGuard       `execute`  turnCond Left (marker 5) error exit0

    --- Collector ---
    -- search
    collectorFind   `defineAs` Sense Ahead foodFind otherFind food -- turn Back enter0
    otherFind       `defineAs` Sense Ahead blockedFind (Move collectorFind blockedFind) home
    foodFind        `defineAs` Move (PickUp collectorFound foodLost) blockedFind
    foodLost        `execute`  turnCond Left food foodFind blockedFind
    blockedFind     `execute`  randomDirection collectorFind
    -- return
    collectorFound  `defineAs` Sense Ahead enter0 (Move collectorFound blockedFound) home
    blockedFound    `execute`  randomDirection collectorFound

    -- Enter home after food found
    --  Dont close in to centre when you can scan another ant with food there
    enter0          `defineAs` Sense Ahead enter0Scan (Turn Right enter0) (marker 0)
    enter0Scan      `execute`  turnCond Left (Not (And (marker 0) friendWithFood)) enter0Move enter0
    enter0Move      `defineAs` Move enter1 enter0
    enter1          `defineAs` Sense Ahead enter1Scan (Turn Right enter1) (marker 1)
    enter1Scan      `execute`  turnCond Left (Not (And (marker 1) friendWithFood)) enter1Move enter1
    enter1Move      `defineAs` Move enter2 enter1
    enter2          `defineAs` Sense Ahead enter2Scan (Turn Right enter2) (marker 2)
    enter2Scan      `execute`  turnCond Left (Not (And (marker 2) friendWithFood)) enter2Move enter2
    enter2Move      `defineAs` Move enter3 enter2
    enter3          `defineAs` Sense RightAhead (Sense Ahead enter3Sign (Turn Left enter3) (marker 3)) (Turn Left enter3) (marker 3)
    enter3Sign      `defineAs` Sense RightAhead enter3Move (Turn Left enter3) (And (marker 3) (marker 2))
    enter3Move      `defineAs` Move enter4 enter3
    enter4          `defineAs` Sense Ahead enter4Move (Turn Right enter4) (And (marker 4) noAnts)
    enter4Move      `defineAs` Move (Drop exit4) enter4

    -- Exit home after food dropped or after start
    exit4           `defineAs` Sense LeftAhead exit4Sign (Turn Right exit4) (marker 3)
    exit4Sign       `defineAs` Sense LeftAhead (Sense Ahead exit4Move exit4 (marker 3)) (Turn Right exit4) (And (marker 3) (marker 4))
    exit4Move       `defineAs` Move exit3 exit4 -- Wait for the defender in front of the defender that leaves you through
    exit3           `defineAs` Sense Ahead exit3Move (Turn Right exit3) (And (marker 2) noAnts)
    exit3Move       `defineAs` Move exit2 exit3 -- Wait for a free location in circle 2
    exit2           `defineAs` Sense Ahead exit2Move (Turn Right exit2) (And (marker 1) noAnts)
    exit2Move       `defineAs` Move exit1 exit2 -- Wait for a free location in circle 1
    exit1           `defineAs` Sense Ahead exit1Move (Turn Right exit1) (And (marker 0) noAnts)
    exit1Move       `defineAs` Move exit0 exit1 -- Wait for a free location in circle 0
    exit0           `defineAs` Sense Ahead exit0Move (Turn Right exit0) (And notHome noAnts)
    exit0Move       `defineAs` Move collectorFind exit0 -- Wait for a free location in outside of your home

    --- Defender ---
    -- patrols clockwise on patches 3
    -- patrol:
    --      align Ahead with patch 3 in direction
    --      scan for enemy on a scannable patches 4 or a patch 3 when on a corner
    --          kill
    --      scan for ant without food on patch 4, which is the RightAhead
    --          exit
    --      scan for ant with food on patch 2, which is the LeftAhead
    --          enter
    --      move to patch in front, a patch 3
    --          -- possible ant wants to enter or exit through your patch
    --          -- there is a possible ant in front of you, you just left it there
    --          -- don't care if defender could move
    --          patrol
    defender        `defineAs` Move defender4on3 defender -- Move all defenders onto patches with a 3
    defender4on3    `execute`  turnCond Right (marker 3) scanEnemy error -- align head with patch 3
    scanEnemy       `defineAs` Sense RightAhead kill (Turn Right (Sense RightAhead kill (Turn Left scanExit) enemyAnts)) enemyAnts
    scanExit        `defineAs` Sense RightAhead exitMark scanEnter friend
    scanEnter       `defineAs` Sense LeftAhead enterMark patrolD friendWithFood
    patrolD         `defineAs` Move defender4on3 defender4on3

    -- enemy RightAhead
    -- kill:
    --      ideas?
    kill            `defineAs` error -- not yet implemented

    -- ant has dropped food on patch 4 and wants to search again
    -- exit:
    --      mark own patch with a 4, this means that ant on patch 4 may exit on the front of this defender
    --      wait until there is no ant on the patch 4 RightAhead of the defender
    --      unmark own patch with a 4
    exitMark        `defineAs` Mark 4 exitSpin
    exitSpin        `defineAs` Sense RightAhead exitSpin exitUnmark friend
    exitUnmark      `defineAs` Unmark 4 patrolD

    -- ant with food on patch 2 wants to drop food on a patch 4
    -- enter:
    --      mark own patch with a 2, this means that ant on patch 2 may enter on the front of this defender
    --      wait until there is no ant on the patch 4 LeftAhead of the defender
    --      unmark own patch with a 2
    enterMark        `defineAs` Mark 2 enterSpin
    enterSpin        `defineAs` Sense RightAhead enterSpin enterUnmark friend
    enterUnmark      `defineAs` Unmark 2 patrolD

    --- Centre Defender ---
    -- sits with all food on patch 5
    -- other ants drop food on a patch 4
    -- cdefender will:
    --      enter that patch 4
    --      pickup the food
    --      return to patch 5
    --      drop the food
    --      continue scanning
    -- cdefender wont:
    --      execute the pickup if enemy is in scanning range
    cdefender       `defineAs` Sense Ahead foodCD (Turn Right cdefender) foodNoAnts -- Find pickupable food
    foodCD          `execute`  turnCond Right enemyAnts cdefender checkPickupCD -- Do not execute pickup if enemyAnt is detected, wait for the kill *Muhahaaa*
    checkPickupCD   `defineAs` Sense Ahead pickupCD cdefender foodNoAnts -- Food is not pickupable anymore, too bad try to find another pickupable food
    pickupCD        `defineAs` Move (PickUp returnCD returnCD) cdefender -- Execute pickup as quickly as possible
    returnCD        `execute`  turn Back patch5Blocked
    -- this is a highly unlikely state
    -- a full sweep of surrounding is made testing for enemies before exiting patch 5
    -- friendlies should not enter patch 5 at all
    -- enemies can enter the patch with the following action: a double move from a patch 3, only 50% of the patches allow such a simple move other patches need a turn as well
    patch5Blocked   `defineAs` Move cdefender patch5Blocked -- intruder detected stealing from base centre if this loops, keep trying to enter patch 5

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
