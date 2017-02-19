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
import qualified Data.Set as S

makeGameState :: Options -> IO GameState
makeGameState options =
   do let theWorld   = getWorld options
          redInstr   = getRedInstructions options
          blackInstr = getBlackInstructions options

      (pm, foodPos, foodParticles) <- populateWorld theWorld

      return $ GameState
         { world             = theWorld
         , redInstructions   = redInstr
         , blackInstructions = blackInstr
         , antPositions      = pm
         , randoms           = randomStream (getSeed options)
         , roundNumber       = 0
         , foodAdmin         = noFood { remaining = foodParticles, locations = foodPos }
         }

populateWorld :: World -> IO (AntPositions, S.Set Pos, Int)
populateWorld theWorld =
   do list <- getAssocs theWorld
      (nrOfAnts, pm, foodSet, nrOfFood) <- foldM op (0, [], S.empty, 0) list
      arr <- newListArray (0, nrOfAnts - 1) (reverse pm)
      return (arr, foodSet, nrOfFood)
 where
   op this@(i, pm, fm, f) (pos, cell)
      | food cell > 0 = return (i, pm, S.insert pos fm, f + food cell)
      | otherwise =
           case anthill cell of
              Nothing ->
                 return this
              Just c ->
                 do writeArray theWorld pos (cell { antInCell = Just (makeAnt i c) })
                    return (i+1, Just pos : pm, fm, f)

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

dumpRound :: Sim ()
dumpRound =
   do w     <- gets world
      list  <- liftIO $ getAssocs w
      i     <- gets roundNumber
      liftIO $ putStrLn ((unlines $ ("After round " ++ show i ++ "...") : map showCell list))