module Language.Compiler (
    MarkerNumber,
    InvChance,
    SenseDir(..),
    LeftOrRight(..),
    Condition(..),
    Instruction(..),

    genCode,
    genIR,
    start,

    optimize,
    ) where

import Prelude hiding (Left, Right)

import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

import qualified Language.Fragment as F
import qualified Language.Instruction as In
import Language.Instruction hiding (Instruction(..))

data Instruction
    = Function String Instruction
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
    parseIns (F.Goto uid)                             = Function (show uid) (parseIns (frag Map.! uid))

genCode :: Instruction -> [In.Instruction]
genCode ins = let (_, _, instructions) = compile ins (0, Map.empty) in instructions

--       ...             Next add, Possible functioncalls       Called state, Updated functioncalls,   Add this code to output
compile :: Instruction -> (AntState, Map.Map String AntState) -> (AntState,     Map.Map String AntState, [In.Instruction])
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

{-
The default program that is implemented recursively
defaultProgram' :: [Instruction]
defaultProgram' = [ Sense Ahead 1 3 Food -- state 0: [SEARCH] is there food in front of me?
                  , Move 2 0 -- state 1: YES: move onto food (return to state 0 on failure)
                  , PickUp 8 0 -- state 2: pick up food and jump to state 8 (or 0 on failure)
                  , Flip 3 4 5 -- state 3: NO: choose whether to...
                  , Turn Left 0 -- state 4: turn left and return to state 0
                  , Flip 2 6 7  -- state 5: ...or...
                  , Turn Right 0 -- state 6: turn right and return to state 0
                  , Move 0 3 -- state 7: ...or move forward and return to state 0 (or 3 on failure)
                  , Sense Ahead 9 11 Home -- state 8: [GO HOME] is the cell in front of me my anthill?
                  , Move 10 8 -- state 9: YES: move onto anthill
                  , Drop 0 -- state 10: drop food and return to searching
                  , Flip 3 12 13 -- state 11: NO: choose whether to...
                  , Turn Left 8 -- state 12: turn left and return to state 8
                  , Flip 2 14 15 -- state 13: ...or...
                  , Turn Right 8 -- state 14: turn right and return to state 8
                  , Move 8 11 -- state 15: ...or move forward and return to state 8
                  ]
-}

start :: Instruction
start = Function "start" (Sense Ahead pickupFood search Food)

pickupFood :: Instruction
pickupFood = Function "pickupFood" (Move (PickUp goHome start) start)

search :: Instruction
search = Function "search" (Flip 3 (Turn Left start) (Flip 2 (Turn Right start) (Move start search)))

goHome :: Instruction
goHome = Function "goHome" (Sense Ahead foundHome notHome Home)

notHome :: Instruction
notHome = Function "notHome" (Flip 3 (Turn Left goHome) (Flip 2 (Turn Right goHome) (Move goHome notHome)))

foundHome :: Instruction
foundHome = Function "foundHome" (Move (Drop start) goHome)

-- Functions: available in Language.Functions
{-
data Directions = LeftLeft | DirLeft | DirAhead | DirRight | RightRight | Back

turn :: Directions -> Instruction -> Instruction
turn LeftLeft nextIns = Turn Left (turn DirLeft nextIns)
turn DirLeft nextIns = Turn Left nextIns
turn DirAhead nextIns = nextIns
turn DirRight nextIns = Turn Right nextIns
turn RightRight nextIns = Turn Right (turn DirRight nextIns)
turn Back nextIns = Turn Right (turn RightRight nextIns)

senseDir :: Directions -> Condition -> Instruction -> Instruction -> Instruction
senseDir LeftLeft cond trueIns falseIns = Turn Left (senseDir DirLeft cond (Turn Right trueIns) (Turn Right falseIns))
senseDir DirLeft cond trueIns falseIns = Sense LeftAhead trueIns falseIns cond
senseDir DirAhead cond trueIns falseIns = Sense Ahead trueIns falseIns cond
senseDir DirRight cond trueIns falseIns = Sense RightAhead trueIns falseIns cond
senseDir RightRight cond trueIns falseIns = Turn Right (senseDir DirRight cond (Turn Left trueIns) (Turn Left falseIns))
senseDir Back cond trueIns falseIns = Turn Right (senseDir RightRight cond (Turn Left trueIns) (Turn Left falseIns))

turnCond :: LeftOrRight -> Condition -> Instruction -> Instruction -> Instruction
turnCond lorr cond trueIns falseIns = turnCond' 6
    where   turnCond' 0 = falseIns
            turnCond' n = Sense Ahead trueIns (Turn lorr (turnCond' (n - 1))) cond
-}
