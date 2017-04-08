{-# LANGUAGE OverloadedStrings #-}
module Vindinium.Play
        ( playTraining
        , playArena
        )
    where

import Data.Text (append)
import qualified Control.Monad.Trans.State as ST

import Vindinium.Types
import Vindinium.Api

playTraining :: Maybe Int -> Maybe Board -> Bot s -> s -> Vindinium State
playTraining mt mb b bs = startTraining mt mb >>= logViewUrl >>= playLoop b bs

playArena :: Bot s -> s -> Vindinium State
playArena b bs = startArena >>= logViewUrl >>= playLoop b bs

playLoop :: Bot s -> s -> State -> Vindinium State
playLoop bot botState state =
    if (gameFinished . stateGame) state
        then return state
        else do
           (dir, s) <- (ST.runStateT $ bot state) botState
           newState <- move state dir
           playLoop bot s newState

logViewUrl :: State -> Vindinium State
logViewUrl s = logInfo fullMessage >> return s
   where
      fullMessage = "Replay URL: " `append` stateViewUrl s
