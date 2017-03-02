{-# LANGUAGE NamedFieldPuns #-}

module Language.Fragment (
    MarkerNumber,
    InvChance,
    SenseDir(..),
    LeftOrRight(..),
    Condition(..),
    Instruction(..),
    DSLState(..),
    Program(..),
    ProgramBuilder,
    ProgramBuildError(..),
    buildProgram,
    declare,
    defineAs,
    define,
) where

import Prelude hiding (Left, Right)
import Control.Monad.State
import Control.Monad.Identity

import qualified Data.Map.Strict as Map
import qualified Language.Compiler as Co
import qualified Prelude as P

import Language.Compiler hiding (Instruction(..))

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
    deriving Show


type ProgramBuilder a = State DSLState a

data DSLState = DSLState {
    maybeEntryPoint :: Maybe Int,
    freshLabel :: Int,
    instructions :: Map.Map Int Instruction
} deriving Show

data Program = Program {
    pEntryPoint :: Int,
    pInstructions :: Map.Map Int Instruction
}

{- Basic building blocks -}

declare :: ProgramBuilder Instruction
declare = do state@(DSLState { freshLabel }) <- get
             put (state { freshLabel = freshLabel + 1})
             return (Goto freshLabel)

defineAs :: Instruction -> Instruction -> ProgramBuilder ()
defineAs (Goto d) i = do (state@DSLState { instructions }) <- get
                         put (state { instructions = Map.insert d i instructions })

define :: Instruction -> ProgramBuilder Instruction
define i = do
    label <- declare
    label `defineAs` i
    return label

setEntryPoint :: Instruction -> ProgramBuilder ()
setEntryPoint (Goto i) = do
    state@(DSLState entryPoint _ _) <- get
    case entryPoint of
        Just ep -> error "Attempt to replace entripoint" -- FIXME: should we use the Exception monad transformer instead of this?
        Nothing -> put $ state { maybeEntryPoint = Just i }

{- Turning the ProgramBuilder into a real Program -}

data ProgramBuildError
    = MissingEntryPoint
    | UndefinedLabel Int

instance Show ProgramBuildError where
    show MissingEntryPoint  = "missing entry point"
    show (UndefinedLabel x) = "there is a goto to label " ++ show x ++ ", but said label does not correspond to any instruction"

buildProgram :: ProgramBuilder () -> Either ProgramBuildError Program
buildProgram p = let (DSLState entry _ instrs) = execState p (DSLState Nothing 0 Map.empty)
                 in case entry of
                      Just ep -> checkProgram $ Program ep instrs
                      Nothing -> P.Left MissingEntryPoint

{- Before returning the program, we better make sure it is properly defined -}

checkProgram :: Program -> Either ProgramBuildError Program
checkProgram = P.Right

{- TODO, check:

* All gotos point to defined instructions
* ?

-}