module Main where

import Ant (program)
import Language.Compiler (compileProgram)
import Language.Instruction(showInstruction)

-- Dumps the compiled program to stdout
main :: IO ()
main = putStrLn $ unlines $ map showInstruction $ compileProgram program
