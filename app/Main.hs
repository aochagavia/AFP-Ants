module Main where

import Lib
import Simulator.Options
import Simulator.Runner
import qualified Worlds

main :: IO ()
main = let red = fragmentProgram
           black = fragmentProgram
           world = Worlds.sample0
           options = buildOptions <$> red <*> black <*> world <*> pure 100000
       in options >>= simulate
