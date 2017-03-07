{-# LANGUAGE NamedFieldPuns #-}

module Language.Compiler (
    MarkerNumber,
    InvChance,
    SenseDir(..),
    LeftOrRight(..),
    Condition(..),
    Instruction(..),

    genCode,
    genIR
    ) where

import Prelude hiding (Left, Right)

import Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Language.Fragment as F
import qualified Language.Instruction as In
import Language.Instruction hiding (Instruction(..))

-- We should be able to get rid of this...
data Instruction
    = Function Int Instruction
    | Sense SenseDir Instruction Instruction Condition
    | Mark MarkerNumber Instruction
    | Unmark MarkerNumber Instruction
    | PickUp Instruction Instruction
    | Drop Instruction
    | Turn LeftOrRight Instruction
    | Move Instruction Instruction
    | Flip InvChance Instruction Instruction
    deriving Show

genIR :: F.Program -> Instruction
genIR (F.Program entry frag) = parseIns (F.Goto entry)
    where
    parseIns (F.Sense senseDir trueIns falseIns cond) = Sense senseDir (parseIns trueIns) (parseIns falseIns) cond
    parseIns (F.Mark markNum ins)                     = Mark markNum (parseIns ins)
    parseIns (F.Unmark markerNum ins)                 = Unmark markerNum (parseIns ins)
    parseIns (F.PickUp trueIns falseIns)              = PickUp (parseIns trueIns) (parseIns falseIns)
    parseIns (F.Drop ins)                             = Drop (parseIns ins)
    parseIns (F.Turn lorr ins)                        = Turn lorr (parseIns ins)
    parseIns (F.Move trueIns falseIns)                = Move (parseIns trueIns) (parseIns falseIns)
    parseIns (F.Flip invChance trueIns falseIns)      = Flip invChance (parseIns trueIns) (parseIns falseIns)
    parseIns (F.Goto uid)                             = Function uid (parseIns (frag Map.! uid))

{- CompileState and related functions -}

type CompileState = State (Map.Map F.Label AntState) (AntState, [In.Instruction])

{- Compiler code -}

genCode :: Instruction -> [In.Instruction]
genCode ins = snd $ fst $ runState (compile ins 0) Map.empty

compile :: Instruction -> AntState -> CompileState
compile (Function label instr)      = functionCall label instr
compile (Sense senseDir f1 f2 cond) = doubleBranch (\callF1 callF2 -> In.Sense senseDir callF1 callF2 cond) f1 f2
compile (PickUp f1 f2)              = doubleBranch In.PickUp f1 f2
compile (Move f1 f2)                = doubleBranch In.Move f1 f2
compile (Flip invChance f1 f2)      = doubleBranch (In.Flip invChance) f1 f2
compile (Mark markNumber f)         = singleBranch (In.Mark markNumber) f
compile (Unmark markNumber f)       = singleBranch (In.Unmark markNumber) f
compile (Drop f)                    = singleBranch In.Drop f
compile (Turn lorr f)               = singleBranch (In.Turn lorr) f

doubleBranch :: (AntState -> AntState -> In.Instruction) -> Instruction -> Instruction -> AntState -> CompileState
doubleBranch toInstruction f1 f2 stateNumber = do
    (callF1, instructions) <- compile f1 (stateNumber + 1)
    (callF2, instructions') <- compile f2 (stateNumber + 1 + length instructions)
    return (stateNumber, toInstruction callF1 callF2 : instructions ++ instructions')

singleBranch :: (AntState -> In.Instruction) -> Instruction -> AntState -> CompileState
singleBranch toInstruction f stateNumber = do
    (call, instructions) <- compile f (stateNumber + 1)
    return (stateNumber, toInstruction call : instructions)

functionCall :: F.Label -> Instruction -> AntState -> CompileState
functionCall label instr stateNumber = do
    generatedFragments <- get
    case Map.lookup label generatedFragments of
        Just state -> return (stateNumber, []) -- Function is already generated -> no code added -> function state returned (the goto)
        Nothing    -> do -- Functioncall added to generatedFragments -> code gets added in the compile of the instruction (the goto is now available in the environment)
            put $ Map.insert label stateNumber generatedFragments
            compile instr stateNumber
