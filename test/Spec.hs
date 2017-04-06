import Prelude hiding (Either(..))

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
