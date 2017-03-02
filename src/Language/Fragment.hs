module Language.Fragment (
    MarkerNumber,
    InvChance,
    SenseDir(..),
    LeftOrRight(..),
    Condition(..),
    Instruction(..),

    DSLState,
    declare,
    defineAs,
    define,

    genIR,
    program,
) where

import Prelude hiding (Left, Right) -- FIXME
import Control.Monad.State
import Control.Monad.Identity

import qualified Data.Map as Map

import Language.Compiler hiding (Instruction(..))
import qualified Language.Compiler as Co

-- import Instruction

data Instruction
    = Sense SenseDir Instruction Instruction Condition
    | Mark MarkerNumber Instruction
    | Unmark MarkerNumber Instruction
    | PickUp Instruction Instruction
    | Drop Instruction
    | Turn LeftOrRight Instruction
    | Move Instruction Instruction
    | Flip InvChance Instruction Instruction
    | Goto Int
    -- | Inline Function Instruction
    deriving Show

data DSLState = DSLState {
    freshLabel :: Int,
    instructions :: [(Int, Instruction)]
} deriving Show

declare :: State DSLState Instruction
declare = do (DSLState u is) <- get
             put (DSLState (u+1) is)
             return (Goto u)

defineAs :: Instruction -> Instruction -> State DSLState ()
defineAs (Goto d) i = do (DSLState u is) <- get
                         put (DSLState u ((d,i):is))

define :: Instruction -> State DSLState Instruction
define i = do
    label <- declare
    label `defineAs` i
    return label

program :: State DSLState ()
program = do
    -- Definitions
    start      <- declare
    pickupFood <- declare
    search     <- declare
    goHome     <- declare
    notHome    <- declare
    foundHome  <- declare

    -- Bodies
    start      `defineAs` Sense Ahead pickupFood search Food
    pickupFood `defineAs` Move (PickUp goHome start) start
    search     `defineAs` Flip 3 (Turn Left start) (Flip 2 (Turn Right start) (Move start search))
    goHome     `defineAs` Sense Ahead foundHome notHome Home
    notHome    `defineAs` Flip 3 (Turn Left goHome) (Flip 2 (Turn Right goHome) (Move goHome notHome))
    foundHome  `defineAs` Move (Drop start) goHome

-- buildProgram program

buildProgram :: State DSLState () -> [(Int, Instruction)]
buildProgram p = let DSLState _ is = execState p (DSLState 0 []) in is

genIR :: State DSLState () -> Co.Instruction
genIR state = toIR fragments
    where   fragments = buildProgram state
            toIR frag = parseIns (Goto 0) -- Assuming fragment with uid 0 is entry point
                where   parseIns (Sense senseDir trueIns falseIns cond) = Co.Sense senseDir (parseIns trueIns) (parseIns falseIns) cond
                        parseIns (Mark markNum ins)                     = Co.Mark markNum (parseIns ins)
                        parseIns (Unmark markerNum ins)                 = Co.Unmark markerNum (parseIns ins)
                        parseIns (PickUp trueIns falseIns)              = Co.PickUp (parseIns trueIns) (parseIns falseIns)
                        parseIns (Drop ins)                             = Co.Drop (parseIns ins)
                        parseIns (Turn lorr ins)                        = Co.Turn lorr (parseIns ins)
                        parseIns (Move trueIns falseIns)                = Co.Move (parseIns trueIns) (parseIns falseIns)
                        parseIns (Flip invChance trueIns falseIns)      = Co.Flip invChance (parseIns trueIns) (parseIns falseIns)
                        parseIns (Goto uid)                             = Co.Function (show uid) (parseIns (Map.fromList frag Map.! uid))

--nameFragment :: Name -> AnonFragment -> Fragment
--nameFragment name (AnonFragment instrs) = Fragment name instrs

--forever :: Function -> State DSLState Instruction
--forever function = do
--    label <- declare
--    label `defineAs` Inline function label
--    return label

{-
{- Example program -}
program :: Program
program = Program main []

main :: Fragment
main = nameFragment (Name "main") $ forever $ sequenceT instructions
    where
    instructions = [ walkUntilFoodFound
                   , Function [ PickUp TEnd TEnd ]
                   , turnAround
                   , walkUntilBaseFound
                   , Function [ Drop TEnd ]
                   ]

                   -}
