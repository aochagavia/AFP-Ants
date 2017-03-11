{-# LANGUAGE NamedFieldPuns #-}

module Language.Fragment (
    MarkerNumber,
    InvChance,
    SenseDir(..),
    LeftOrRight(..),
    BoolExpr(..),
    Condition(..),
    Fragment(..),
    Program(..),
    ProgramBuilder,
    ProgramBuildError(..),
    Label,
    buildProgram,
    declare,
    execute,
    defineAs,
    define,
    setEntryPoint,
) where

import Prelude hiding (Left, Right)
import Control.Monad.State
import Control.Monad.Writer
import Data.List ((\\), sort)

import Language.Instruction hiding (Instruction(..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Prelude as P

{- The AST -}

data Program = Program {
    pEntryPoint :: Label,
    pFragments :: Map.Map Label Fragment
}

data BoolExpr
    = Cond Condition
    | Not BoolExpr
    | And BoolExpr BoolExpr
    | Or BoolExpr BoolExpr
    deriving (Eq, Show)

data Fragment
    = Sense SenseDir Fragment Fragment BoolExpr
    | Mark MarkerNumber Fragment
    | Unmark MarkerNumber Fragment
    | PickUp Fragment Fragment
    | Drop Fragment
    | Turn LeftOrRight Fragment
    | Move Fragment Fragment
    | Flip InvChance Fragment Fragment
    | Goto Int
    deriving (Eq, Show)

type FragmentAlgebra frag =
    (
        -- Sense
        SenseDir -> frag -> frag -> BoolExpr -> frag,
        -- Mark
        MarkerNumber -> frag -> frag,
        -- Unmark
        MarkerNumber -> frag -> frag,
        -- PickUp
        frag -> frag -> frag,
        -- Drop
        frag -> frag,
        -- Turn
        LeftOrRight -> frag -> frag,
        -- Move
        frag -> frag -> frag,
        -- Flip
        InvChance -> frag -> frag -> frag,
        -- Goto
        Int -> frag
    )

foldFragment :: Fragment -> FragmentAlgebra frag -> frag
foldFragment fragment (sense, mark, unmark, pickUp, drop,  turn, move, flip', goto) = f fragment
    where f (Sense dir f1 f2 cond) = sense dir (f f1) (f f2) cond
          f (Mark x f1) = mark x (f f1)
          f (Unmark x f1) = unmark x (f f1)
          f (PickUp f1 f2) = pickUp (f f1) (f f2)
          f (Drop f1) = drop $ f f1
          f (Turn lr f1) = turn lr $ f f1
          f (Move f1 f2) = move (f f1) (f f2)
          f (Flip ic f1 f2) = flip' ic (f f1) (f f2)
          f (Goto x) = goto x

type ProgramAlgebra prog lfrag frag =
    (
        -- A function to fold pFragments
        [lfrag] -> prog,
        -- A function to combine labels and fragment results
        Label -> frag -> lfrag,
        FragmentAlgebra frag
    )

foldProgram :: Program -> ProgramAlgebra prog lfrag frag -> prog
foldProgram Program{pFragments} (frags, lfrag, fragAlg) = frags $ map reduceLFrag $ Map.toList pFragments
    where reduceLFrag (k, v) = lfrag k $ foldFragment v fragAlg

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

instance Monoid DSLOutput where
    mempty = DSLOutput Set.empty [] Map.empty
    mappend (DSLOutput df1 ep1 fr1) (DSLOutput df2 ep2 fr2) =
        let duplicates  = df1 <> df2 <> (Set.fromList (Map.keys fr1) `Set.intersection` Set.fromList (Map.keys fr2))
            entryPoints = ep1 <> ep2
            fragments   = fr1 <> fr2
        in DSLOutput duplicates entryPoints fragments

{- Basic building blocks -}

declare :: ProgramBuilder Fragment
declare = do freshLabel <- get
             put (freshLabel + 1)
             return (Goto freshLabel)

execute :: Fragment -> ProgramBuilder Fragment -> ProgramBuilder ()
execute goto@(Goto _) exec = do
    label <- exec
    defineAs goto label
    return ()

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
    | UnusedLabel Label
    deriving (Eq, Ord)

instance Show ProgramBuildError where
    show (DeadCode x)             = "fragment with label " ++ show x ++ " is defined but never used"
    show (MultipleDefinitions x)  = "multiple definitions for label " ++ show x
    show (MultipleEntryPoints xs) = "multiple entrypoints: " ++ show xs
    show NoEntryPoint             = "no entry point defined"
    show (UndefinedLabel x)       = "goto targeting undefined label " ++ show x
    show (UnusedLabel x)          = "label " ++ show x ++ " is declared but not defined"

buildProgram :: ProgramBuilder () -> Either [ProgramBuildError] Program
buildProgram programBuilder =
    let (DSLOutput duplicates entryPoints frags, topLabel) = runState (execWriterT programBuilder) zeroLabel
    in case entryPoints of -- Note: we treat multiple or missing entry points as a fatal error
        []       -> P.Left [NoEntryPoint]
        (x:y:zs) -> P.Left [MultipleEntryPoints entryPoints]
        [ep]     -> checkNoDuplicates duplicates
                    >>= const (return (Program ep frags))
                    >>= checkGotosDefined
                    >>= checkNoUnusedLabels topLabel
                    >>= checkNoDeadCode ep

{- Before returning the program, we better make sure it is properly defined -}

checkNoDuplicates :: Set.Set Int -> Either [ProgramBuildError] ()
checkNoDuplicates dups = case map MultipleDefinitions $ Set.toList dups of
                           [] -> return ()
                           xs -> P.Left xs

getFragmentGotoTargets:: Fragment -> [Label]
getFragmentGotoTargets f = foldFragment f (sense, mark, unmark, pickUp, drop, turn, move, flip, goto)
    where
        sense _ f1 f2 _ = f1 ++ f2
        mark = const id
        unmark = const id
        pickUp = (++)
        drop = id
        turn = const id
        move = (++)
        flip = const (++)
        goto = (:[])

getGotoTargets :: Program -> [Label] --  map (pFragments p) getFragmentGotoTargets
getGotoTargets p = foldProgram p (concat, const id, (sense, mark, unmark, pickUp, drop, turn, move, flip, goto))
    where
        sense _ f1 f2 _ = f1 ++ f2
        mark = const id
        unmark = const id
        pickUp = (++)
        drop = id
        turn = const id
        move = (++)
        flip = const (++)
        goto = (:[])

getUsedGotoTargets :: Program -> [Label]
getUsedGotoTargets p = Set.toList $ f [pEntryPoint p] Set.empty
    where f [] ys     = ys
          f (x:xs) ys = let ys' = Set.union (Set.singleton x) ys in
                        let xs' = xs ++ getFragmentGotoTargets (pFragments p Map.! x) in
                        f (dropWhile (`Set.member` ys') xs') ys'

getFragmentLabels :: Program -> [Label]
getFragmentLabels (Program _ fragments) = Map.keys fragments

checkGotosDefined :: Program -> Either [ProgramBuildError] Program
checkGotosDefined p@(Program entryPoint fragments) =
    let targetSet = Set.fromList $ getGotoTargets p
        labelSet = Set.fromList $ getFragmentLabels p
        undefinedGotos = targetSet `Set.difference` labelSet
    in case map UndefinedLabel $ Set.toList undefinedGotos of
        [] -> return p
        xs -> P.Left xs

checkNoDeadCode :: Label -> Program -> Either [ProgramBuildError] Program
checkNoDeadCode ep p =
    let ds = Map.keys (pFragments p) \\ sort (ep : getUsedGotoTargets p) in
    if null ds then P.Right p
    else P.Left (map DeadCode ds)

-- topLabel is the output of the State monad and therefore equals the number of declared labels
checkNoUnusedLabels :: Label -> Program -> Either [ProgramBuildError] Program
checkNoUnusedLabels topLabel p =
    let us = [0..topLabel-1] \\ Map.keys (pFragments p) in
    if null us then P.Right p
    else P.Left (map UnusedLabel us)
