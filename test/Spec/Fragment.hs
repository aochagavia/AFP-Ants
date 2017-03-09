module Spec.Fragment where

import Test.QuickCheck
import Util

import Prelude hiding (Either(..))
import qualified Language.Examples as Examples
import Language.Fragment

testFragment :: IO ()
testFragment = do
    quickCheck checkNoEntryPoint
    quickCheck checkExampleProgram
    quickCheck checkUndefinedGoto
    quickCheck checkNoUnusedLabels
    quickCheck checkNoUnusedLabelsEntryPoint

checkNoEntryPoint :: Bool
checkNoEntryPoint = errors == [NoEntryPoint]
    where errors = eitherToList $ buildProgram program
          program = do
                    start <- declare
                    start `defineAs` Turn Right start

checkUndefinedGoto :: Bool
checkUndefinedGoto = errors == [UndefinedLabel 42]
    where errors = eitherToList $ buildProgram program
          program = do
                    start <- define $ Turn Right (Goto 42)
                    setEntryPoint start

checkNoUnusedLabels :: Property
checkNoUnusedLabels = counterexample (show errors ++ " == [UnusedLabel 1]") (errors == [UnusedLabel 1])
    where errors = eitherToList $ buildProgram program
          program = do
                    la <- declare
                    lb <- declare -- Oops. No define.
                    lc <- declare
                    la `defineAs` Turn Right lc
                    lc `defineAs` Turn Right la
                    setEntryPoint la

checkNoUnusedLabelsEntryPoint :: Property
checkNoUnusedLabelsEntryPoint = counterexample (show errors ++ " == [UnusedLabel 0]") (errors == [UnusedLabel 0])
    where errors = eitherToList $ buildProgram program
          program = do
                    la <- declare
                    setEntryPoint la

checkExampleProgram :: Bool
checkExampleProgram = isRight $ buildProgram Examples.program
