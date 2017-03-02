import qualified Worlds
import qualified Language.Instruction as In
import qualified Language.Compiler as Co
import Test.QuickCheck

main :: IO ()
main = do
    definedWorlds
    quickCheck optimizeInstructionsSense
    quickCheck optimizeInstructionsFlip

definedWorlds :: IO ()
definedWorlds = sequence_ worlds
    where worlds = [Worlds.sample0]

optimizeInstructionsSense :: Bool
optimizeInstructionsSense = [] == Co.optimize [In.Sense In.Here 0 0 In.Home]

optimizeInstructionsFlip :: Bool
optimizeInstructionsFlip = [] == Co.optimize [In.Flip 2 0 0]
