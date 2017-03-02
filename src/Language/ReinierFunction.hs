module Language.Function (
    MarkerNumber,
    InvChance,
    SenseDir(..),
    LeftOrRight(..),
    Condition(..),
    Instruction(..),

    run,
    test,

    start,
    ) where

import Prelude hiding (Left, Right)

import qualified Data.Map as Map
import qualified Instruction as In
import Instruction hiding (Instruction(..))

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

run :: Instruction -> [In.Instruction]
run ins = let (_, _, instructions) = parse ins (0, Map.empty) in instructions

--       ...             Next add, Possible functioncalls       Called state, Updated functioncalls,   Add this code to output
parse :: Instruction -> (AntState, Map.Map String AntState) -> (AntState,     Map.Map String AntState, [In.Instruction])
-- functioncall
parse (Function name instr)       state@(nextState, functioncalls) = case Map.lookup name functioncalls of
                                                                          Just state -> (state, functioncalls, []) -- Function is already available -> no code added -> function state returned (the goto)
                                                                          Nothing    -> parse instr (nextState, Map.insert name nextState functioncalls) -- Functioncall added to functioncalls -> code gets added in the parse of the instruction (the goto is now available in the environment)
-- double call
parse (Sense senseDir f1 f2 cond) state@(nextState, functioncalls) = let (callF1, functioncalls', instructions)   = parse f1 (nextState + 1, functioncalls) in
                                                                     let (callF2, functioncalls'', instructions') = parse f2 (nextState + 1 + length instructions, functioncalls') in
                                                                         (nextState, functioncalls'', In.Sense senseDir callF1 callF2 cond : instructions ++ instructions')
parse (PickUp f1 f2)              state@(nextState, functioncalls) = let (callF1, functioncalls', instructions)   = parse f1 (nextState + 1, functioncalls) in
                                                                     let (callF2, functioncalls'', instructions') = parse f2 (nextState + 1 + length instructions, functioncalls') in
                                                                         (nextState, functioncalls'', In.PickUp callF1 callF2 : instructions ++ instructions')
parse (Move f1 f2)                state@(nextState, functioncalls) = let (callF1, functioncalls', instructions)   = parse f1 (nextState + 1, functioncalls) in
                                                                     let (callF2, functioncalls'', instructions') = parse f2 (nextState + 1 + length instructions, functioncalls') in
                                                                         (nextState, functioncalls'', In.Move callF1 callF2 : instructions ++ instructions')
parse (Flip invChance f1 f2)      state@(nextState, functioncalls) = let (callF1, functioncalls', instructions)   = parse f1 (nextState + 1, functioncalls) in
                                                                     let (callF2, functioncalls'', instructions') = parse f2 (nextState + 1 + length instructions, functioncalls') in
                                                                         (nextState, functioncalls'', In.Flip invChance callF1 callF2 : instructions ++ instructions')
-- single call
parse (Mark markNumber f)         state@(nextState, functioncalls) = let (call, functioncalls', instructions)     = parse f (nextState + 1, functioncalls) in
                                                                         (nextState, functioncalls', In.Mark markNumber call : instructions)
parse (Unmark markNumber f)       state@(nextState, functioncalls) = let (call, functioncalls', instructions)     = parse f (nextState + 1, functioncalls) in
                                                                         (nextState, functioncalls', In.Unmark markNumber call : instructions)
parse (Drop f)                    state@(nextState, functioncalls) = let (call, functioncalls', instructions)     = parse f (nextState + 1, functioncalls) in
                                                                         (nextState, functioncalls', In.Drop call : instructions)
                                                                    --let (f', env') = parse f (x + 1, map, ins ++ [In.Drop f']) in (x, env')
parse (Turn lorr f)               state@(nextState, functioncalls) = let (call, functioncalls', instructions)     = parse f (nextState + 1, functioncalls) in
                                                                         (nextState, functioncalls', In.Turn lorr call : instructions)
                                                                    --let (f', env') = parse f (x + 1, map, ins ++ [In.Turn lorr f']) in (x, env')


test, test1 :: Instruction
test = Function "test" (Move test1 test)
test1 = Function "test1" (Drop test)

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

-- Idea: use a uniqSupply like Monad to get unique identifiers for each definition.
-- Then define "defineAs" somehow.
{-
program = do
    -- Definitions
    start      <- typeGoto
    pickupFood <- typeGoto
    search     <- typeGoto
    goHome     <- typeGoto
    foundHome  <- typeGoto

    -- Bodies
    start      `defineAs` Sense Ahead pickupFood search Food
    pickupFood `defineAs` Move (PickUp goHome start) start
    search     `defineAs` Flip 3 (Turn Left start) (Flip 2 (Turn Right start) (Move start search))
    goHome     `defineAs` Sense Ahead foundHome (Flip 3 (Turn Left goHome) (Flip 2 (Turn Right goHome) (Move goHome))) Home
    foundHome  `defineAs` Move (Drop start) goHome
-}

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