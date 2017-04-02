{-# LANGUAGE OverloadedStrings #-}
module Vindinium.Play
        ( playTraining
        , playArena
        )
    where

import Data.Text (append)

import Vindinium.Types
import Vindinium.Api

playTraining :: Maybe Int -> Maybe Board -> Bot -> Vindinium State
playTraining mt mb b = startTraining mt mb >>= logViewUrl >>= playLoop b

playArena :: Bot -> Vindinium State
playArena b = startArena >>= logViewUrl >>= playLoop b

playLoop :: Bot -> State -> Vindinium State
playLoop bot state =
    if (gameFinished . stateGame) state
        then return state
        else do
            newState <- bot state >>= move state
            playLoop bot newState

logViewUrl :: State -> Vindinium State
logViewUrl s = logInfo fullMessage >> return s
   where
      fullMessage = "Replay URL: " `append` stateViewUrl s
