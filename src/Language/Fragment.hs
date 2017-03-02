module Language.Fragment where

import Prelude hiding (Left, Right) -- FIXME
import Control.Monad.State
import Control.Monad.Identity

import Instruction hiding (Instruction(..))
import Language.Function (Function)

import qualified Language.Function as Fn

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
    | Inline Function Instruction
    deriving Show

data DSLState = DSLState Int [(Int, Instruction)] deriving Show

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
    search     `defineAs` (Flip 3 (Turn Left start) (Flip 2 (Turn Right start) (Move start search)))
    goHome     `defineAs` Sense Ahead foundHome notHome Home
    notHome    `defineAs` Flip 3 (Turn Left goHome) (Flip 2 (Turn Right goHome) (Move goHome notHome))
    foundHome  `defineAs` Move (Drop start) goHome

buildProgram :: State DSLState () -> [(Int, Instruction)]
buildProgram p = let DSLState _ is = snd $ (runState p) (DSLState 0 []) in is

-- buildProgram program

--nameFragment :: Name -> AnonFragment -> Fragment
--nameFragment name (AnonFragment instrs) = Fragment name instrs

forever :: Function -> State DSLState Instruction
forever function = do
    label <- declare
    label `defineAs` Inline function label
    return label

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