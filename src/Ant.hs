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
    -- Definitions ---
    -- number of ants = 91
    -- number of circles = 6 (0..5)
    start           <- declare
    error           <- declare -- non reachable state
    -- init phase
    selectCircle0   <- declare
    selectCircle1   <- declare
    selectCircle2   <- declare
    selectCircle3   <- declare
    selectCircle4   <- declare
    selectCircle5   <- declare
    -- scouts
    scout           <- declare
    errorScout      <- declare
    -- collector
    collector       <- declare
    errorCollector  <- declare

    pickupFood      <- declare
    search          <- declare
    goHome          <- declare
    notHome         <- declare
    foundHome       <- declare

    -- defender
    defender        <- declare
    defender4on3    <- declare
    patrolD         <- declare

    -- centre defender
    cdefender       <- declare
    foodCD          <- declare
    checkPickupCD   <- declare
    pickupCD        <- declare
    returnCD        <- declare
    patch5Blocked   <- declare

    --- Bodies ---
    start           `defineAs` selectCircle0
    error           `defineAs` Drop error -- non reachable state -- infinite loop
    --- init phase
    selectCircle0   `execute` turnCond Left (Not (Cond Home)) (Mark 0 scout) selectCircle1
    selectCircle1   `execute` turnCond Left (And (Cond Home) (Cond $ Marker 0)) (Mark 1 collector) selectCircle2
    selectCircle2   `execute` turnCond Left (And (Cond Home) (Cond $ Marker 1)) (Mark 2 collector) selectCircle3
    selectCircle3   `execute` turnCond Left (And (Cond Home) (Cond $ Marker 2)) (Mark 3 collector) selectCircle4
    selectCircle4   `execute` turnCond Left (And (Cond Home) (Cond $ Marker 3)) (Mark 4 defender) selectCircle5
    selectCircle5   `execute` turnCond Left (And (Cond Home) (Cond $ Marker 4)) (Mark 5 cdefender) error

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

    --- scouts
    scout           `defineAs` Move scout errorScout
    errorScout      `defineAs` error
    -- collector
    collector       `defineAs` Move collector errorCollector
    errorCollector  `defineAs` error


    pickupFood      `defineAs` Move (PickUp goHome start) start
    search          `defineAs` Flip 3 (Turn Left start) (Flip 2 (Turn Right start) (Move start search))
    goHome          `defineAs` Sense Ahead foundHome notHome (Cond Home)
    notHome         `defineAs` Flip 3 (Turn Left goHome) (Flip 2 (Turn Right goHome) (Move goHome notHome))
    foundHome       `defineAs` Move (Drop start) goHome

    --- Defender ---
    -- patrols clockwise on patches 3
    -- patrol:
    --      align Ahead with patch 3 in direction
    --      scan for enemy on a scannable patches 4
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
    -- kill:
    --

    -- ant has dropped food on patch 4 and wants to search again
    -- exit:
    --      mark own patch with a 4, this means that ant on patch 4 may exit on the front of this defender
    --      wait until there is no ant on the patch 4 RightAhead of the defender
    --      unmark own patch with a 4

    -- ant with food on patch 2 wants to drop food on a patch 4
    -- enter:
    --      mark own patch with a 2, this means that ant on patch 2 may enter on the front of this defender
    --      wait until there is no ant on the patch 4 LeftAhead of the defender
    --      unmark own patch with a 2

    defender        `defineAs` Move defender4on3 defender -- Move all defenders onto patches with a 3
    defender4on3    `execute` turnCond Right (Cond $ Marker 3) patrolD error
    patrolD         `defineAs` Move defender4on3 defender4on3

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
    cdefender       `execute` turnCond Right foodNoAnts foodCD cdefender -- Find pickupable food
    foodCD          `execute` turnCond Right enemyAnts cdefender checkPickupCD -- Do not execute pickup if enemyAnt is detected, wait for the kill *Muhahaaa*
    checkPickupCD   `defineAs` Sense Ahead pickupCD cdefender foodNoAnts -- Food is not pickupable anymore, too bad try to find another pickupable food
    pickupCD        `defineAs` Move (PickUp returnCD returnCD) cdefender -- Execute pickup as quickly as possible
    returnCD        `execute` turn Back (Move cdefender patch5Blocked)
    -- this is a highly unlikely state
    -- a full sweep of surrounding is made testing for enemies before exiting patch 5
    -- friendlies should not enter patch 5 at all
    -- enemies can enter the patch with the following action: a double move from a patch 3, only 50% of the patches allow such a simple move other patches need a turn as well
    patch5Blocked   `defineAs` Move cdefender patch5Blocked -- intruder detected stealing from base centre, keep trying to enter patch 5

    --- Entry point ---
    setEntryPoint start

enemyAnts, friendlyAnts, ants, food, foodOrAnts, foodNoAnts :: BoolExpr
enemyAnts = Or (Cond Foe) (Cond FoeWithFood)
friendlyAnts = Or (Cond Friend) (Cond FriendWithFood)
ants = Or enemyAnts friendlyAnts
food = Cond Food
foodOrAnts = And food ants
foodNoAnts = And food (Not ants)

fragmentProgram :: [I.Instruction]
fragmentProgram = compileProgram programReinier
