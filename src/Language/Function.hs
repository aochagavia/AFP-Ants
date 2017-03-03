module Language.Function where

import Prelude hiding (Left, Right)
import Control.Monad.State

import Language.Fragment
import Language.Instruction hiding (Instruction(..))

-- A function requires an instruction, which is executed when the function ends
type Function = Fragment -> ProgramBuilder Fragment

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
choose fs = undefined -- FIXME: how do you use flip in a fair way?

{- Example functions -}

walkUntilBaseFound, walkUntilFoodFound :: Function
walkUntilBaseFound = walkUntilCond Home
walkUntilFoodFound = walkUntilCond Food

walkUntilCond :: Condition -> Function
walkUntilCond cond ret = do
    start <- declare
    walk <- declare
    start `defineAs` Sense Here ret walk cond
    walk `defineAs` Move start walk
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
