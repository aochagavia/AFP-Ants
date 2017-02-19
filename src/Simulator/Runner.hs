module Simulator.Runner where

import Simulator
import Simulator.Options
import Simulator.GamePlay

simulate :: Options -> IO ()
simulate myOptions =
   do putStrLn $ "random seed: " ++ show (getSeed myOptions)
      game  <- makeGameState myOptions
      final <- runSimulator (allRounds myOptions) game
      putStrLn (showScore myOptions final)

showScore :: Options -> GameState -> String
showScore myOptions game =
   unlines
      [ "Score: round " ++ show (roundNumber game)
      , f ("(red) "  ) ++ ": " ++ show (redScore   $ foodAdmin game)
      , f ("(black) ") ++ ": " ++ show (blackScore $ foodAdmin game)
      ]
 where
   f = take 30 . (++repeat ' ') . ("   "++)
