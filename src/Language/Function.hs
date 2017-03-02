module Language.Function where

import Prelude hiding (Left, Right)
import Control.Monad.State

import Language.Fragment
import Language.Instruction hiding (Instruction(..))

-- A function requires an instruction, which is executed when the function ends
type Function = Instruction -> State DSLState Instruction
-- A function with a conditional statement requires two instruction
type CondFunction = Instruction -> Instruction -> State DSLState Instruction

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

-- Combine the functions in such a way that, at runtime, one of them is run randomly
choose :: [Function] -> Function
choose [] = error "cannot choose 0 functions"
choose [f] = f
choose count@(f:fs) nextIns = Flip (length count) (f nextIns) (choose fs nextIns) -- Flip in a fair way, 1 / number of possible choices

-- Functions to make turning and sensing easier

data Directions = LeftLeft | DirLeft | DirAhead | DirRight | RightRight | Back

turn :: Directions -> Function
turn dir nextIns = define (turn' dir nextIns)
    where   turn' :: Directions -> Instruction -> Instruction
            turn' LeftLeft      nextIns = Turn Left (turn' DirLeft nextIns)
            turn' DirLeft       nextIns = Turn Left nextIns
            turn' DirAhead      nextIns = nextIns
            turn' DirRight      nextIns = Turn Right nextIns
            turn' RightRight    nextIns = Turn Right (turn' DirRight nextIns)
            turn' Back          nextIns = Turn Right (turn' RightRight nextIns)

turnCond :: LeftOrRight -> Condition -> CondFunction
turnCond lorr cond trueIns falseIns = define $ turnCond' 6
    where   turnCond' 0 = falseIns
            turnCond' n = Sense Ahead trueIns (Turn lorr (turnCond' (n - 1))) cond

senseDir :: Directions -> Condition -> CondFunction
senseDir dir cond trueIns falseIns = define (senseDir' dir cond trueIns falseIns)
    where   senseDir' :: Directions -> Condition -> Instruction -> Instruction -> Instruction
            senseDir' LeftLeft      cond trueIns falseIns = Turn Left (senseDir' DirLeft cond (Turn Right trueIns) (Turn Right falseIns))
            senseDir' DirLeft       cond trueIns falseIns = Sense LeftAhead trueIns falseIns cond
            senseDir' DirAhead      cond trueIns falseIns = Sense Ahead trueIns falseIns cond
            senseDir' DirRight      cond trueIns falseIns = Sense RightAhead trueIns falseIns cond
            senseDir' RightRight    cond trueIns falseIns = Turn Right (senseDir' DirRight cond (Turn Left trueIns) (Turn Left falseIns))
            senseDir' Back          cond trueIns falseIns = Turn Right (senseDir' RightRight cond (Turn Left trueIns) (Turn Left falseIns))

{- Example functions -}

walkUntilBaseFound, walkUntilFoodFound :: Function
walkUntilBaseFound = walkUntilCond Home
walkUntilFoodFound = walkUntilCond Food

walkUntilCond :: Condition -> Function
walkUntilCond cond ret = do
    start <- declare
    walk <- declare
    start `defineAs` Sense Here ret walk cond
    walk `defineAs` Move start walk -- warning don't use these functions (ants get locked against walls)
    return start

turnAround :: Function
turnAround = times 3 (return . Turn Left)

{- A non-terminating program -}

forever :: Function -> State DSLState Instruction
forever mkFunction = do
    start <- declare
    function <- mkFunction start
    start `defineAs` function
    return start
