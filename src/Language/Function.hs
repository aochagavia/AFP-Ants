module Language.Function where

import Prelude hiding (Left, Right)
import Control.Monad.State

import Language.Fragment

-- A function requires an instruction, which is executed when the function ends
type Function = Fragment -> ProgramBuilder Fragment
-- A function with a conditional statement requires two instruction
type CondFunction = Fragment -> Fragment -> ProgramBuilder Fragment

-- Creates a new function produced by repeating the function code i times
times :: Int -> Function -> Function
times i = sequenceF . replicate i

-- Creates a new function produced by concatenating the functions in the list
sequenceF :: [Function] -> Function
sequenceF [] = error "cannot sequenceF 0 functions"
sequenceF [f] = f
sequenceF (f:fs) = \ret -> do
        nextFn <- sequenceF fs ret
        f nextFn


chooseCond :: [CondFunction] -> CondFunction
chooseCond []       _     _      = error "cannot choose 0"
chooseCond [f]      true  false  = choose [f true] false
chooseCond choices  true  false  = choose (map (\y -> y true) choices) false

-- Combine the functions in such a way that, at runtime, one of them is run randomly
choose :: [Function] -> Function
choose [] _ = error "cannot choose 0 functions"
choose [f] nextIns = f nextIns
choose choices@(f:fs) nextIns = let count = length choices in
                                if even count then evenlength count else oddlength count
    where   evenlength count = let (left, right) = splitAt (count `div` 2) choices in do
                leftChoices <- choose left nextIns
                rightChoices <- choose right nextIns
                define $ Flip 2 leftChoices rightChoices
            oddlength count = do
                chosen <- f nextIns
                other <- choose fs nextIns
                define $ Flip count chosen other -- Flip in a fair way, 1 / number of possible choices

-- Functions to make turning and sensing easier

data Directions = LeftLeft | DirLeft | DirAhead | DirRight | RightRight | Back

directions :: [Directions]
directions = [LeftLeft, DirLeft, DirAhead, DirRight, RightRight, Back]

turn :: Directions -> Function
turn dir nextIns = define (turn' dir nextIns)
    where   turn' :: Directions -> Fragment -> Fragment
            turn' LeftLeft      nextIns = Turn Left (turn' DirLeft nextIns)
            turn' DirLeft       nextIns = Turn Left nextIns
            turn' DirAhead      nextIns = nextIns
            turn' DirRight      nextIns = Turn Right nextIns
            turn' RightRight    nextIns = Turn Right (turn' DirRight nextIns)
            turn' Back          nextIns = Turn Right (turn' RightRight nextIns)

turnCond :: LeftOrRight -> BoolExpr -> CondFunction
turnCond lorr cond trueIns falseIns = define $ turnCond' 6
    where   turnCond' 0 = falseIns
            turnCond' n = Sense Ahead trueIns (Turn lorr (turnCond' (n - 1))) cond

turnUntil :: LeftOrRight -> BoolExpr -> Function
turnUntil lorr cond ret = do
  sense <- declare
  turn <- declare
  sense `defineAs` Sense Ahead ret turn cond
  turn `defineAs` Turn lorr sense
  return sense

{-turnUntil lorr cond ret = do
  sense <- declare
  sense `defineAs` Sense Ahead ret sense cond
  move <- declare
  move `defineAs` Sense
  return sense-}


--turnUntil lorr cond true = turnCond lorr (Not cond) (turnUntil lorr cond true) true
--turnUntil lorr cond true = Sense Ahead true (turnUntil lorr cond true) cond

--turnUntil lorr cond true = turnCond lorr (Not cond) (turnUntil lorr cond true) true

senseDir :: Directions -> BoolExpr -> CondFunction
senseDir dir cond trueIns falseIns = define (senseDir' dir cond trueIns falseIns)
    where   senseDir' :: Directions -> BoolExpr -> Fragment -> Fragment -> Fragment
            senseDir' LeftLeft      cond trueIns falseIns = Turn Left (senseDir' DirLeft cond (Turn Right trueIns) (Turn Right falseIns))
            senseDir' DirLeft       cond trueIns falseIns = Sense LeftAhead trueIns falseIns cond
            senseDir' DirAhead      cond trueIns falseIns = Sense Ahead trueIns falseIns cond
            senseDir' DirRight      cond trueIns falseIns = Sense RightAhead trueIns falseIns cond
            senseDir' RightRight    cond trueIns falseIns = Turn Right (senseDir' DirRight cond (Turn Left trueIns) (Turn Left falseIns))
            senseDir' Back          cond trueIns falseIns = Turn Right (senseDir' RightRight cond (Turn Left trueIns) (Turn Left falseIns))

{- Example functions -}

randomDirection :: Function
randomDirection = choose $ map turn directions

walkUntilBaseFound, walkUntilFoodFound :: CondFunction
walkUntilBaseFound = walkUntilCond (Cond Home)
walkUntilFoodFound = walkUntilCond (Cond Food)

randomWalkUntilCondition :: BoolExpr -> Function
randomWalkUntilCondition cond ret = do
  random    <- declare
  randomDir <- randomDirection random
  walk      <- walkUntilCond cond ret randomDir
  random    `defineAs` walk
  return random

randomWalkUntilBaseFound :: Function
randomWalkUntilBaseFound = randomWalkUntilCondition (Cond Home)

randomWalkUntilFoodFound :: Function
randomWalkUntilFoodFound = randomWalkUntilCondition (Cond Food)

walkUntilCond :: BoolExpr -> CondFunction
walkUntilCond cond true false = do
    start   <- declare
    walk    <- declare
    start   `defineAs`  Sense Here true walk cond
    walk    `defineAs`  Move start false
    return start

turnAround :: Function
turnAround = times 3 (return . Turn Left)

{- A non-terminating program -}

forever :: Function -> ProgramBuilder Fragment
forever mkFunction = do
    start <- declare
    function <- mkFunction start
    start `defineAs` function
    return start
