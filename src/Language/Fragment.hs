{-# LANGUAGE NamedFieldPuns #-}

module Language.Fragment (
    MarkerNumber,
    InvChance,
    SenseDir(..),
    LeftOrRight(..),
    Condition(..),
    Fragment(..),
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

import Language.Instruction hiding (Instruction(..))

import qualified Data.Map.Strict as Map
import qualified Prelude as P

data Fragment
    = Sense SenseDir Fragment Fragment Condition
    | Mark MarkerNumber Fragment
    | Unmark MarkerNumber Fragment
    | PickUp Fragment Fragment
    | Drop Fragment
    | Turn LeftOrRight Fragment
    | Move Fragment Fragment
    | Flip InvChance Fragment Fragment
    | Goto Int
    deriving Show

type ProgramBuilder a = State DSLState a
type Label = Int

data DSLState = DSLState {
    maybeEntryPoint :: Maybe Int,
    freshLabel :: Int,
    instructions :: Map.Map Int Fragment
} deriving Show

data Program = Program {
    pEntryPoint :: Int,
    pFragments :: Map.Map Int Fragment
}

{- Basic building blocks -}

declare :: ProgramBuilder Fragment
declare = do state@(DSLState { freshLabel }) <- get
             put (state { freshLabel = freshLabel + 1})
             return (Goto freshLabel)

defineAs :: Fragment -> Fragment -> ProgramBuilder ()
defineAs (Goto d) i = do
    (state@DSLState { instructions }) <- get
    -- TODO: use Map.insertLookupWithKey to ensure no double definitions happen
    put (state { instructions = Map.insert d i instructions })

define :: Fragment -> ProgramBuilder Fragment
define i = do
    label <- declare
    label `defineAs` i
    return label

setEntryPoint :: Fragment -> ProgramBuilder ()
setEntryPoint (Goto i) = do
    state@(DSLState entryPoint _ _) <- get
    case entryPoint of
        Just ep -> error "Attempt to replace entripoint" -- FIXME: should we use the Exception monad transformer instead of this?
        Nothing -> put $ state { maybeEntryPoint = Just i }

{- Turning the ProgramBuilder into a real Program -}

data ProgramBuildError
    = DeadCode Label
    | DoubleDefinition Label
    | MissingEntryPoint
    | UndefinedLabel Label

instance Show ProgramBuildError where
    show (DeadCode x)         = "fragment with label " ++ show x ++ " is defined but never used"
    show (DoubleDefinition x) = "multiple definitions for label " ++ show x
    show MissingEntryPoint    = "missing entry point"
    show (UndefinedLabel x)   = "goto targeting undefined label " ++ show x

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
* No dead code (fragments that are defined but not used)
* No unused labels (fragments that are declared but not defined)

-}