module Language.Compiler (
    compileProgram,
    genCodeOpt,
    ) where

import Debug.Trace

import Language.Codegen
import Language.Fragment hiding (LeftOrRight(..))
import Language.Instruction (Instruction)
import Language.Optimizer

{- Compiler code -}

compileProgram :: ProgramBuilder () -> [Instruction]
compileProgram = genCode . fromRight . buildProgram
    where
    fromRight (Right r) = r
    fromRight (Left error) = traceShow error undefined

genCodeOpt :: Program -> [Instruction]
genCodeOpt = optimize . genCode
