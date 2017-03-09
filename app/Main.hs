module Main where

import Prelude hiding (Either(..))

import Lib
import Language.Compiler
import Language.Fragment
import Simulator.Options
import Simulator.Runner
import qualified Worlds

-- Dumps the compiled program to stdout
main :: IO ()
main = putStrLn $ unlines $ map show $ compileProgram program

-- Runs the simulation
runSim :: IO ()
runSim = let red = fragmentProgram
             black = fragmentProgram
             world = Worlds.sample0
             options = buildOptions <$> red <*> black <*> world <*> pure 100000
         in options >>= simulate

program :: ProgramBuilder ()
program = do
    -- Definitions
    start      <- declare
    pickupFood <- declare
    search     <- declare
    goHome     <- declare
    notHome    <- declare
    foundHome  <- declare

    -- Bodies
    start      `defineAs` Sense Ahead pickupFood search Food
    pickupFood `defineAs` Move (PickUp goHome start) start
    search     `defineAs` Flip 3 (Turn Left start) (Flip 2 (Turn Right start) (Move start search))
    goHome     `defineAs` Sense Ahead foundHome notHome Home
    notHome    `defineAs` Flip 3 (Turn Left goHome) (Flip 2 (Turn Right goHome) (Move goHome notHome))
    foundHome `defineAs` Move (Drop start) goHome

    -- Entry point
    setEntryPoint start
