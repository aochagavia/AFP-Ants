module Ant (
    fragmentProgram,
    program
) where

import Prelude hiding (Either(..))

import Control.Monad.State
import Language.Compiler (genCode)
import Language.Fragment
import Language.Function
import Language.Instruction hiding (Instruction(..))
import Language.Optimizer

import qualified Language.Instruction as I
import qualified Prelude as P

program :: ProgramBuilder ()
program = do
    -- Definitions
    start        <- declare
    walkFromHome <- declare
    fromHome     <- declare
    pickupFood   <- declare
    search       <- declare
    goHome       <- declare
    notHome      <- declare
    foundHome    <- declare

    -- Bodies
    start      `defineAs` walkFromHome
    walkFromHome `defineAs` Move (Sense Here walkFromHome fromHome Home) walkFromHome
    fromHome     `defineAs` turnCond Left Sense Ahead (Turn Right search) search Home
    pickupFood `defineAs` Move (PickUp goHome start) start
    search     `defineAs` Flip 3 (Turn Left start) (Flip 2 (Turn Right start) (Move start search))
    goHome     `defineAs` Sense Ahead foundHome notHome Home
    notHome    `defineAs` Flip 3 (Turn Left goHome) (Flip 2 (Turn Right goHome) (Move goHome notHome))
    foundHome  `defineAs` Move (Drop start) goHome

    -- Entry point
    setEntryPoint start

fragmentProgram :: [I.Instruction]
fragmentProgram = let (P.Right code) = genCode <$> buildProgram program in code