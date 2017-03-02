module Language.Build (
    genIR
) where

import Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Prelude as P
import Prelude hiding (Left, Right)

import qualified Language.Compiler as Co
import Language.Fragment

genIR :: Program -> Co.Instruction
genIR (Program entry frag) = parseIns (Goto entry)
    where
    parseIns (Sense senseDir trueIns falseIns cond) = Co.Sense senseDir (parseIns trueIns) (parseIns falseIns) cond
    parseIns (Mark markNum ins)                     = Co.Mark markNum (parseIns ins)
    parseIns (Unmark markerNum ins)                 = Co.Unmark markerNum (parseIns ins)
    parseIns (PickUp trueIns falseIns)              = Co.PickUp (parseIns trueIns) (parseIns falseIns)
    parseIns (Drop ins)                             = Co.Drop (parseIns ins)
    parseIns (Turn lorr ins)                        = Co.Turn lorr (parseIns ins)
    parseIns (Move trueIns falseIns)                = Co.Move (parseIns trueIns) (parseIns falseIns)
    parseIns (Flip invChance trueIns falseIns)      = Co.Flip invChance (parseIns trueIns) (parseIns falseIns)
    parseIns (Goto uid)                             = Co.Function (show uid) (parseIns (frag Map.! uid))
