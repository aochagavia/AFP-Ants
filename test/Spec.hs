import Prelude hiding (Either(..))
import Test.QuickCheck
import Test.QuickCheck.Property as P

import qualified Language.Examples as Examples
import qualified Prelude as P
import qualified Worlds
import Language.Fragment

import Debug.Trace

main :: IO ()
main = do
    definedWorlds
    quickCheck checkNoEntryPoint
    quickCheck checkExampleProgram
    quickCheck checkUndefinedGoto
    quickCheck checkNoUnusedLabels
    quickCheck checkNoUnusedLabelsEntryPoint

definedWorlds :: IO ()
definedWorlds = sequence_ worlds
    where worlds = [Worlds.sample0]

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

{- Utility functions -}
isRight :: P.Either a b -> Bool
isRight (P.Right _) = True
isRight (P.Left _)  = False

eitherToList :: P.Either [a] b -> [a]
eitherToList (P.Right _) = []
eitherToList (P.Left xs) = xs
