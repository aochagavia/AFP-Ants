{-# LANGUAGE NamedFieldPuns #-}

module Language.Compiler (
    MarkerNumber,
    InvChance,
    SenseDir(..),
    LeftOrRight(..),
    Condition(..),

    compileProgram,
    genCode,
    optimize,
    ) where

import Prelude hiding (Left, Right)
import qualified Prelude as P

import Data.Maybe (mapMaybe)
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Language.Fragment
import qualified Language.Instruction as In
import Language.Instruction (AntState)

{- CompileState -}

-- There are two things going on here:
-- * A state monad, to keep track of the generated code
-- * A reader monad, to query the map of labelled fragments (see the call to `ask` in the Goto code)
type LabelledFragments = (Map.Map Int Fragment)
type GeneratedFragments = (Map.Map Label AntState)
type CompileState = ReaderT LabelledFragments (State GeneratedFragments) (AntState, [In.Instruction])

{- Compiler code -}

compileProgram :: ProgramBuilder () -> [In.Instruction]
compileProgram = genCode . fromRight . buildProgram
    where
    fromRight (P.Right r) = r

genCode :: Program -> [In.Instruction]
genCode (Program start fragments) = optimize . snd $ evalState (runReaderT (compile (Goto start) 0) fragments) Map.empty

compile :: Fragment -> AntState -> CompileState
compile (Goto label)                = functionCall label
compile (Sense senseDir f1 f2 cond) = doubleBranch (\callF1 callF2 -> In.Sense senseDir callF1 callF2 cond) f1 f2
compile (PickUp f1 f2)              = doubleBranch In.PickUp f1 f2
compile (Move f1 f2)                = doubleBranch In.Move f1 f2
compile (Flip invChance f1 f2)      = doubleBranch (In.Flip invChance) f1 f2
compile (Mark markNumber f)         = singleBranch (In.Mark markNumber) f
compile (Unmark markNumber f)       = singleBranch (In.Unmark markNumber) f
compile (Drop f)                    = singleBranch In.Drop f
compile (Turn lorr f)               = singleBranch (In.Turn lorr) f

doubleBranch :: (AntState -> AntState -> In.Instruction) -> Fragment -> Fragment -> AntState -> CompileState
doubleBranch toInstruction f1 f2 stateNumber = do
    (callF1, instructions) <- compile f1 (stateNumber + 1)
    (callF2, instructions') <- compile f2 (stateNumber + 1 + length instructions)
    return (stateNumber, toInstruction callF1 callF2 : instructions ++ instructions')

singleBranch :: (AntState -> In.Instruction) -> Fragment -> AntState -> CompileState
singleBranch toInstruction f stateNumber = do
    (call, instructions) <- compile f (stateNumber + 1)
    return (stateNumber, toInstruction call : instructions)
functionCall :: Label -> AntState -> CompileState
functionCall label stateNumber = do
    generatedFragments <- get
    case Map.lookup label generatedFragments of
        Just state -> return (state, []) -- Function is already generated -> no code added -> function state returned (the goto)
        Nothing    -> do -- Functioncall added to generatedFragments -> code gets added in the compile of the instruction (the goto is now available in the environment)
            put $ Map.insert label stateNumber generatedFragments
            labelledFragments <- ask
            compile (labelledFragments Map.! label) stateNumber

-- Optimmizations
optimize :: [In.Instruction] -> [In.Instruction]
optimize instructions = mapMaybe inscompact instructions
    where   filter (In.Sense senseDir trueState falseState condition)   = (trueState == falseState, trueState)
            filter (In.Flip invChance chooseState otherState)           = (chooseState == otherState, chooseState)
            filter _                                                    = (False, -1)

            instructionreshuffle _ []     = []
            instructionreshuffle n (i:is) = let (deadbranch, nextState) =  filter i in if deadbranch
                then n:instructionreshuffle (n + 1) is
                else n:instructionreshuffle n is

            reshuffleamount = instructionreshuffle 0 instructions

            -- actions that have two the same branches and no sideeffect can be savely removed
            inscompact (In.Sense senseDir trueState falseState condition)  = if trueState == falseState
                then Nothing
                else Just $ In.Sense senseDir (newstate trueState) (newstate falseState) condition
            inscompact (In.Flip invChance chooseState otherState)          = if chooseState == otherState
                then Nothing
                else Just $ In.Flip invChance (newstate chooseState) (newstate otherState)
            -- actions can be called with two the same branches but never be deleted because of side effects
            inscompact (In.PickUp trueState falseState)                    = Just $ In.PickUp (newstate trueState) (newstate falseState)
            inscompact (In.Move trueState falseState)                      = Just $ In.Move (newstate trueState) (newstate falseState)
            -- actions don't have the possibility to call the same branch on different conditions, never the less their goto has to be updated
            inscompact (In.Mark markerNumber nextState)                    = Just $ In.Mark markerNumber (newstate nextState)
            inscompact (In.Unmark markerNumber nextState)                  = Just $ In.Unmark markerNumber (newstate nextState)
            inscompact (In.Drop nextState)                                 = Just $ In.Drop (newstate nextState)
            inscompact (In.Turn lorr nextState)                            = Just $ In.Turn lorr (newstate nextState)
            -- calculate newstate based on the shift of the called state
            newstate nextState = nextState - (reshuffleamount !! nextState)
