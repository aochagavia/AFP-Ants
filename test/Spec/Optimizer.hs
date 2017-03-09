module Spec.Optimizer where

import Test.QuickCheck
import Language.Instruction
import Language.Optimizer

testOptimizer :: IO ()
testOptimizer = do
    quickCheck optimizeInstructionsSense
    quickCheck optimizeInstructionsSense'
    quickCheck optimizeInstructionsFlip

optimizeInstructionsSense :: Bool
optimizeInstructionsSense = [] == optimize [Sense Here 0 0 Home]

optimizeInstructionsSense' :: Bool
optimizeInstructionsSense' = [Move 1 1, Drop 0] == optimize [Sense Here 2 2 Home, Drop 0, Move 1 1]

optimizeInstructionsFlip :: Bool
optimizeInstructionsFlip = [] == optimize [Flip 2 0 0]
