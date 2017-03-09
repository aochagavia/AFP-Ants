module Language.Optimizer (optimize, optimize') where

import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, mapMaybe)
import Language.Codegen (genCode)
import Language.Instruction
import qualified Language.Fragment as F

-- Optimizations
optimize :: [Instruction] -> [Instruction]
optimize = genCode . F.Program 0 . fmap remove . toFragMap
    where
    toFragMap = Map.fromList . zip [0..] . map toFragment
    -- Actions that have two the same branches and no side effects can be safely removed
    remove :: F.Fragment -> F.Fragment
    remove f@(F.Sense _ true false _) = if true   == false then true   else f
    remove f@(F.Flip _ choose other)  = if choose == other then choose else f
    remove frag                       = frag

{- Optimization based on state machines (incomplete) -}

optimize' :: [Instruction] -> [Instruction]
optimize' instructions = adjustIndices . fromStateMachine instructionMap . optimizeStateMachine . toStateMachine $ instructions
    where
    instructionMap = Map.fromList (zip [0..] instructions)

{- Adjusting indices -}

-- Note: after optimization, it may be possible that some states don't exist anymore
-- We can no longer assume that state numbers are continuous. Therefore we need to adjust
-- them so they become continuous aga
-- Casually, this seems very similar to the problem the compiler is facing when compiling a program to ant code!
-- We should be able to reuse the code here. Therefore, we transform our low level instructions back into
-- program form, where each instruction gets its own label.
adjustIndices :: Map.Map Int Instruction -> [Instruction]
adjustIndices = genCode . toProgram

toProgram :: Map.Map Int Instruction -> F.Program
toProgram = F.Program 0 . Map.map toFragment

toFragment :: Instruction -> F.Fragment
toFragment (Sense dir f1 f2 cond) = F.Sense dir (F.Goto f1) (F.Goto f2) cond
toFragment (Mark n f)             = F.Mark n (F.Goto f)
toFragment (Unmark n f)           = F.Unmark n (F.Goto f)
toFragment (PickUp f1 f2)         = F.PickUp (F.Goto f1) (F.Goto f2)
toFragment (Drop f)               = F.Drop (F.Goto f)
toFragment (Turn lor f)           = F.Turn lor (F.Goto f)
toFragment (Move f1 f2)           = F.Move (F.Goto f1) (F.Goto f2)
toFragment (Flip chance f1 f2)    = F.Flip chance (F.Goto f1) (F.Goto f2)

{- State machine primitives -}

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

{- State machine optimizations -}

-- WARNING: our state machine representation is unable to reason about side effects.
-- We need to take that into consideration.

-- FIXME: implement something useful
optimizeStateMachine :: Map.Map Int State -> Map.Map Int State
optimizeStateMachine = id
