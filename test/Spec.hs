import Prelude hiding (Either(..))

import qualified Worlds

import Test.QuickCheck

import Spec.Codegen
import Spec.Fragment
import Spec.Optimizer
import Util

main :: IO ()
main = do
    testCodegen
    testFragment
    testOptimizer
    definedWorlds

definedWorlds :: IO ()
definedWorlds = sequence_ worlds
    where worlds = [Worlds.sample0]
