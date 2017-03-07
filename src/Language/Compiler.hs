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

import qualified Data.Map as Map
import qualified Language.Fragment as F
import qualified Language.Instruction as In
import Language.Instruction hiding (Instruction(..))

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

genCode :: Instruction -> [In.Instruction]
genCode ins = let (_, _, instructions) = compile ins (0, Map.empty) in instructions

--       ...             Next add, Possible functioncalls      Called state, Updated functioncalls, Add this code to output
compile :: Instruction -> (AntState, Map.Map Int AntState) -> (AntState,     Map.Map Int AntState, [In.Instruction])
-- functioncall
compile (Function name instr)       state@(nextState, functioncalls) = case Map.lookup name functioncalls of
                                                                          Just state -> (state, functioncalls, []) -- Function is already available -> no code added -> function state returned (the goto)
                                                                          Nothing    -> compile instr (nextState, Map.insert name nextState functioncalls) -- Functioncall added to functioncalls -> code gets added in the compile of the instruction (the goto is now available in the environment)
-- double call
compile (Sense senseDir f1 f2 cond) state@(nextState, functioncalls) = let (callF1, functioncalls', instructions)   = compile f1 (nextState + 1, functioncalls) in
                                                                       let (callF2, functioncalls'', instructions') = compile f2 (nextState + 1 + length instructions, functioncalls') in
                                                                           (nextState, functioncalls'', In.Sense senseDir callF1 callF2 cond : instructions ++ instructions')
compile (PickUp f1 f2)              state@(nextState, functioncalls) = let (callF1, functioncalls', instructions)   = compile f1 (nextState + 1, functioncalls) in
                                                                       let (callF2, functioncalls'', instructions') = compile f2 (nextState + 1 + length instructions, functioncalls') in
                                                                           (nextState, functioncalls'', In.PickUp callF1 callF2 : instructions ++ instructions')
compile (Move f1 f2)                state@(nextState, functioncalls) = let (callF1, functioncalls', instructions)   = compile f1 (nextState + 1, functioncalls) in
                                                                       let (callF2, functioncalls'', instructions') = compile f2 (nextState + 1 + length instructions, functioncalls') in
                                                                           (nextState, functioncalls'', In.Move callF1 callF2 : instructions ++ instructions')
compile (Flip invChance f1 f2)      state@(nextState, functioncalls) = let (callF1, functioncalls', instructions)   = compile f1 (nextState + 1, functioncalls) in
                                                                       let (callF2, functioncalls'', instructions') = compile f2 (nextState + 1 + length instructions, functioncalls') in
                                                                           (nextState, functioncalls'', In.Flip invChance callF1 callF2 : instructions ++ instructions')
-- single call
compile (Mark markNumber f)         state@(nextState, functioncalls) = let (call, functioncalls', instructions)     = compile f (nextState + 1, functioncalls) in
                                                                           (nextState, functioncalls', In.Mark markNumber call : instructions)
compile (Unmark markNumber f)       state@(nextState, functioncalls) = let (call, functioncalls', instructions)     = compile f (nextState + 1, functioncalls) in
                                                                           (nextState, functioncalls', In.Unmark markNumber call : instructions)
compile (Drop f)                    state@(nextState, functioncalls) = let (call, functioncalls', instructions)     = compile f (nextState + 1, functioncalls) in
                                                                           (nextState, functioncalls', In.Drop call : instructions)
compile (Turn lorr f)               state@(nextState, functioncalls) = let (call, functioncalls', instructions)     = compile f (nextState + 1, functioncalls) in
                                                                           (nextState, functioncalls', In.Turn lorr call : instructions)
