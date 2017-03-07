module Language.Optimizer (optimize) where

import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Language.Instruction

optimize :: [Instruction] -> [Instruction]
optimize instructions = adjustIndices . fromStateMachine instructionMap . optimizeStateMachine . toStateMachine $ instructions
    where
    instructionMap = Map.fromList (zip [0..] instructions)

{- Adjusting indices -}

-- Note: after optimization, it may be possible that some states don't exist anymore
-- We can no longer assume that state numbers are continuous. Therefore we need to adjust
-- them so they become continuous again.
-- Casually, this seems very similar to the problem the compiler is facing when compiling a program to ant code!
-- We should be able to reuse the code here...
adjustIndices :: Map.Map Int Instruction -> [Instruction]
adjustIndices = undefined

{- State machine transformations -}

data State
    -- | A state that has only one next state
    = Deterministic Int
    -- | A state that has two next states
    | NonDeterministic Int Int

toStateMachine :: [Instruction] -> Map.Map Int State
toStateMachine = snd . foldl' mkStates (0, Map.empty)
    where
    mkStates :: (Int, Map.Map Int State) -> Instruction -> (Int, Map.Map Int State)
    mkStates (stateNumber, states) instruction = (stateNumber + 1, Map.insert stateNumber (mkState instruction) states)
    mkState :: Instruction -> State
    mkState (Sense _ nextA nextB _) = NonDeterministic nextA nextB
    mkState (Mark _ next) = Deterministic next
    mkState (Unmark _ next) = Deterministic next
    mkState (PickUp nextA nextB) = NonDeterministic nextA nextB
    mkState (Drop next) = Deterministic next
    mkState (Turn _ next) = Deterministic next
    mkState (Move nextA nextB) = NonDeterministic nextA nextB
    mkState (Flip _ nextA nextB) = NonDeterministic nextA nextB

fromStateMachine :: Map.Map Int Instruction -> Map.Map Int State -> Map.Map Int Instruction
fromStateMachine oldInstructions = Map.fromList . map mkInstruction . Map.keys
    where
    mkInstruction :: Int -> (Int, Instruction)
    mkInstruction stateNumber = (stateNumber, fromJust $ Map.lookup stateNumber oldInstructions)

-- FIXME: this optimizer doesn't do anything
optimizeStateMachine :: Map.Map Int State -> Map.Map Int State
optimizeStateMachine = id
