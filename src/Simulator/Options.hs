module Simulator.Options where

import Simulator

import Data.List (isSuffixOf)
import System.Console.GetOpt
import System.Environment
import System.Exit

{-
optionDescription :: [OptDescr Option]
optionDescription =
   [ Option ""  ["dumpfile"] (NoArg DumpFile)                    "Dump trace file"
   , Option ""  ["rounds"]   (ReqArg (NrOfRounds . read) "int")  "Number of rounds (default=100000)"
   , Option ""  ["frate"]    (ReqArg (FrameRate  . read) "int")  "Frames per second (default=5)"
   , Option ""  ["seed"]     (ReqArg (Seed       . read) "int")  "Seed for random numbers (default=12345)"
   , Option "r" ["red"]      (ReqArg (RedFile          ) "file") "Red ant file (mandatory)"
   , Option "b" ["black"]    (ReqArg (BlackFile        ) "file") "Black ant file (mandatory)"
   , Option "w" ["world"]    (ReqArg (WorldFile        ) "file") "World file (mandatory)"
   , Option ""  ["simulate"] (NoArg Simulate)                    "Simulation only (no gui)"
   ]
-}

data Options  = Options
  { getSeed :: Int
  , getNrOfRounds :: Int
  , getFrameRate :: Int
  , getRedInstructions :: AntInstructions
  , getBlackInstructions :: AntInstructions
  , getWorld :: World
  }
  deriving Eq

defaultOptions :: Options
defaultOptions = Options
  { getSeed = 12345
  , getNrOfRounds = 100000
  , getFrameRate = 5
  , getRedInstructions = undefined
  , getBlackInstructions = undefined
  , getWorld = undefined
  }

buildOptions :: AntInstructions -> AntInstructions -> World -> Options
buildOptions red black world = defaultOptions { getRedInstructions = red
                                              , getBlackInstructions = black
                                              , getWorld = world
                                              }

dropEnd :: Int -> [a] -> [a]
dropEnd n xs = take (length xs - n) xs
