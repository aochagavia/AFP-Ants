{-# LANGUAGE NamedFieldPuns #-}

module Language.Fragment (
    MarkerNumber,
    InvChance,
    SenseDir(..),
    LeftOrRight(..),
    Condition(..),
    Fragment(..),
    Program(..),
    ProgramBuilder,
    ProgramBuildError(..),
    buildProgram,
    declare,
    defineAs,
    define,
    setEntryPoint,
) where

import Prelude hiding (Left, Right)
import Control.Monad.State
import Control.Monad.Writer

import Language.Instruction hiding (Instruction(..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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

{- Basic types -}

-- ProgramBuilder uses the Write monad for output (see DSLOutput and calls to the `tell` function)
-- It also uses the State monad to produce fresh labels
type ProgramBuilder a = WriterT DSLOutput (State Int) a
type Label = Int

zeroLabel :: Label
zeroLabel = 0

data DSLOutput = DSLOutput {
    -- | Keeps track of fragments that have been defined more than once
    duplicateFragments :: Set.Set Int,
    -- | Keeps track of all labels that have been used as entry point
    entryPoints :: [Label],
    -- | Keeps track of all defined fragments (duplicates are ignored)
    fragments :: Map.Map Label Fragment
}

data Program = Program {
    pEntryPoint :: Label,
    pFragments :: Map.Map Label Fragment
}

instance Monoid DSLOutput where
    mempty = DSLOutput Set.empty [] Map.empty
    mappend (DSLOutput df1 ep1 fr1) (DSLOutput df2 ep2 fr2) =
        let duplicates  = df1 <> df2 <> Set.fromList (Map.keys fr1) <> Set.fromList (Map.keys fr2)
            entryPoints = ep1 <> ep2
            fragments   = fr1 <> fr2
        in DSLOutput duplicates entryPoints fragments

{- Basic building blocks -}

declare :: ProgramBuilder Fragment
declare = do freshLabel <- get
             put (freshLabel + 1)
             return (Goto freshLabel)

defineAs :: Fragment -> Fragment -> ProgramBuilder ()
defineAs (Goto d) i = tell $ mempty { fragments = Map.singleton d i }

define :: Fragment -> ProgramBuilder Fragment
define i = do
    label <- declare
    label `defineAs` i
    return label

setEntryPoint :: Fragment -> ProgramBuilder ()
setEntryPoint (Goto i) = tell $ mempty { entryPoints = [i] }

{- Turning the ProgramBuilder into a real Program -}

data ProgramBuildError
    = DeadCode Label
    | MultipleDefinitions Label
    | MultipleEntryPoints [Label]
    | NoEntryPoint
    | UndefinedLabel Label
    deriving (Eq, Ord)

instance Show ProgramBuildError where
    show (DeadCode x)         = "fragment with label " ++ show x ++ " is defined but never used"
    show (MultipleDefinitions x)  = "multiple definitions for label " ++ show x
    show (MultipleEntryPoints xs) = "multiple entrypoints: " ++ show xs
    show NoEntryPoint    = "no entry point defined"
    show (UndefinedLabel x)   = "goto targeting undefined label " ++ show x

buildProgram :: ProgramBuilder () -> Either [ProgramBuildError] Program
buildProgram programBuilder =
    let (DSLOutput duplicates entryPoints frags, topLabel) = runState (execWriterT programBuilder) zeroLabel
    in case entryPoints of -- Note: we treat multiple or missing entry points as a fatal error
        []       -> P.Left [NoEntryPoint]
        (x:y:zs) -> P.Left [MultipleEntryPoints entryPoints]
        [ep]     -> checkNoDuplicates duplicates
                    >>= const (return (Program ep frags))
                    >>= checkGotosDefined
                    >>= checkNoDeadCode
                    >>= checkNoUnusedLabels topLabel

{- Before returning the program, we better make sure it is properly defined -}

checkNoDuplicates :: Set.Set Int -> Either [ProgramBuildError] ()
checkNoDuplicates dups = case map MultipleDefinitions $ Set.toList dups of
                           [] -> return ()
                           xs -> P.Left xs

-- FIXME: implement stuff below (consider using an algebra)

-- Note: we could enforce this at compile time if we made a labelling monad
checkGotosDefined :: Program -> Either [ProgramBuildError] Program
checkGotosDefined = return

checkNoDeadCode :: Program -> Either [ProgramBuildError] Program
checkNoDeadCode = return

checkNoUnusedLabels :: Label -> Program -> Either [ProgramBuildError] Program
checkNoUnusedLabels topLabel = return
