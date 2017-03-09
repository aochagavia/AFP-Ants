module Language.Compiler (
    compileProgram,
    genCodeOpt,
    ) where

import Language.Codegen
import Language.Fragment hiding (LeftOrRight(..))
import Language.Instruction (Instruction)
import Language.Optimizer

{- Compiler code -}

compileProgram :: ProgramBuilder () -> [Instruction]
compileProgram = genCode . fromRight . buildProgram
    where
    fromRight (Right r) = r

genCodeOpt :: Program -> [Instruction]
genCodeOpt = optimize . genCode
