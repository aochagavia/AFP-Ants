import Prelude hiding (Either(..))
import Test.QuickCheck

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

checkExampleProgram :: Bool
checkExampleProgram = isRight $ buildProgram Examples.program

{- Utility functions -}
isRight :: P.Either a b -> Bool
isRight (P.Right _) = True
isRight (P.Left _)  = False

eitherToList :: P.Either [a] b -> [a]
eitherToList (P.Right _) = []
eitherToList (P.Left xs) = xs
