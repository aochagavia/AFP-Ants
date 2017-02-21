module Main where

import Lib
import Simulator.Options
import Simulator.Runner
import qualified Worlds

main :: IO ()
main = let red = defaultProgram
           black = defaultProgram
           world = Worlds.sample0
           options = buildOptions <$> red <*> black <*> world <*> pure 100000
       in options >>= simulate
