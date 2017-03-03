import Prelude hiding (Either(..))
import Test.QuickCheck

import qualified Language.Examples as Examples
import qualified Prelude as P
import qualified Worlds
import Language.Fragment

main :: IO ()
main = do
    definedWorlds
    quickCheck checkNoEntryPoint
    quickCheck checkExampleProgram

definedWorlds :: IO ()
definedWorlds = sequence_ worlds
    where worlds = [Worlds.sample0]

checkNoEntryPoint :: Bool
checkNoEntryPoint = errors == [NoEntryPoint]
    where errors = either id (const []) $ buildProgram program
          program = do
                    start <- declare
                    start `defineAs` Turn Right start

checkExampleProgram :: Bool
checkExampleProgram = isRight $ buildProgram Examples.program

{- Utility functions -}
isRight :: P.Either a b -> Bool
isRight (P.Right _) = True
isRight (P.Left _)  = False
