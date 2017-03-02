module Language.Function where

import Prelude hiding (Left, Right)
import Control.Monad.State

import Language.Fragment
import Language.Instruction hiding (Instruction(..))

-- A function requires an instruction, which is executed when the function ends
type Function = Instruction -> State DSLState Instruction

-- Creates a new function produced by repeating the function code i times
times :: Int -> Function -> Function
times i = sequenceF . replicate i

-- Creates a new function produced by concatenating the functions in the list
sequenceF :: [Function] -> Function
sequenceF [] = error "cannot sequenceF 0 templates"
sequenceF (f:[]) = f
sequenceF (f:fs) = \ret -> do
        nextFn <- sequenceF fs ret
        start <- f nextFn
        return start

{- Example functions -}

walkUntilBaseFound, walkUntilFoodFound :: Function
walkUntilBaseFound = walkUntilCond Home
walkUntilFoodFound = walkUntilCond Food

walkUntilCond :: Condition -> Function
walkUntilCond cond = \ret -> do
    start <- declare
    walk <- declare
    start `defineAs` Sense Here ret walk cond
    walk `defineAs` Move start walk
    return start

turnAround :: Function
turnAround = times 3 (\ret -> return $ Turn Left ret)

{- A non-terminating program -}

forever :: Function -> State DSLState Instruction
forever mkFunction = do
    start <- declare
    function <- mkFunction start
    start `defineAs` function
    return start
