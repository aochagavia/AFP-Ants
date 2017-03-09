module Spec.Codegen where

import Debug.Trace
import Test.QuickCheck
import Util

import Language.Compiler
import Language.Fragment
import qualified Language.Instruction as I

testCodegen :: IO ()
testCodegen = do
    quickCheck condWorks
    quickCheck negationWorks

condWorks :: Bool
condWorks = prog == [I.Sense Here 1 0 Home, I.Move 0 0]
    where
    prog = compileProgram $ do
                start <- declare
                move <- define $ Move start start
                start `defineAs` Sense Here move start (Cond Home)

                setEntryPoint start

negationWorks :: Bool
negationWorks = prog == [I.Sense Here 0 1 Home, I.Move 0 0]
    where
    prog = compileProgram $ do
                start <- declare
                move <- define $ Move start start
                start `defineAs` Sense Here move start (Not $ Cond Home)

                setEntryPoint start
