{-# LANGUAGE FlexibleContexts #-}

module Simulator.GamePlay where

import Simulator
import Simulator.ReadWorld
import Simulator.ReadInstructions
import Simulator.Options
import Data.Array.IO
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy

makeGameState :: Options -> IO GameState
makeGameState options =
   do let theWorld   = getWorld options
          redInstr   = getRedInstructions options
          blackInstr = getBlackInstructions options

      pm <- populateWorld theWorld

      return $ GameState
         { world             = theWorld
         , redInstructions   = redInstr
         , blackInstructions = blackInstr
         , antPositions      = pm
         , randoms           = randomStream (getSeed options)
         , roundNumber       = 0
         , foodAdmin         = noFood
         }

populateWorld :: World -> IO AntPositions
populateWorld theWorld =
   do list <- getAssocs theWorld
      (nrOfAnts, pm) <- foldM op (0, []) list
      arr <- newListArray (0, nrOfAnts - 1) (reverse pm)
      return arr
 where
   op this@(i, pm) (pos, cell)
      | food cell > 0 = return (i, pm)
      | otherwise =
           case anthill cell of
              Nothing ->
                 return this
              Just c ->
                 do writeArray theWorld pos (cell { antInCell = Just (makeAnt i c) })
                    return (i+1, Just pos : pm)

makeAnt :: Int -> AntColor -> Ant
makeAnt i c = Ant
   { antId        = i
   , antColor     = c
   , antState     = 0
   , antResting   = 0
   , antDirection = 0
   , antHasFood   = False
   }

allRounds :: Options -> Sim ()
allRounds options = replicateM_ (getNrOfRounds options) (oneRound options)

oneRound :: Options -> Sim ()
oneRound options =
   do rnr <- gets roundNumber
      unless (rnr >= getNrOfRounds options) $
         do modify (\game -> game {roundNumber = roundNumber game + 1})
            pm   <- gets antPositions
            list <- liftIO $ getAssocs pm
            mapM_ step list
