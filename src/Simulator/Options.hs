module Simulator.Options where

import Simulator

import Data.List (isSuffixOf)
import System.Console.GetOpt
import System.Environment
import System.Exit

data Options  = Options
  { getSeed :: Int
  , getNrOfRounds :: Int
  , getRedInstructions :: AntInstructions
  , getBlackInstructions :: AntInstructions
  , getWorld :: World
  }
  deriving Eq

defaultOptions :: Options
defaultOptions = Options
  { getSeed = 12345
  , getNrOfRounds = 100000
  , getRedInstructions = undefined
  , getBlackInstructions = undefined
  , getWorld = undefined
  }

buildOptions :: AntInstructions -> AntInstructions -> World -> Int -> Options
buildOptions red black world rounds = defaultOptions { getRedInstructions = red
                                                     , getBlackInstructions = black
                                                     , getWorld = world
                                                     , getNrOfRounds = rounds
                                                     }
