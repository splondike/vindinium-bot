{-# LANGUAGE OverloadedStrings #-}
module Bot
        ( bot
        )
    where

import Data.Ord (comparing)
import Data.List (sortBy)
import Data.Maybe (catMaybes)

import System.Random (getStdRandom, randomR)
import Data.Maybe (isJust)
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

import qualified BotHelper as BH
import qualified Vindinium as V

import Debug.Trace

bot :: V.Bot
bot state = pickRule [
         ("heal to full", tupleRule (nextToTavern && notFullHealth && haveTavernMoney, goToNearestTavern)),
         ("snipe hero", trySnipeHero),
         ("low health", tupleRule (lowHealth && haveTavernMoney, goToNearestTavern)),
         ("i'm winning", tupleRule (earningRateHighest && wealthLargest, goToNearestTavern)),
         ("go to mine", tupleRule (True, goToNearestMine))
      ]
   where
      pickRule rules = do
         let matches = filter (isJust . snd) rules
         let (name, action) = case matches of
                                 [] -> ("no matching rules", V.Stay)
                                 (name, Just action):_ -> (name, action)
         V.logDebug name
         return action
      tupleRule (True, action) = Just action
      tupleRule _ = Nothing

      trySnipeHero = case filter isSnipable closeHeroes of
                          [] -> Nothing
                          enemy:_ -> if imHealthy then snipeHero enemy else Nothing
         where
            imHealthy = (>50) $ V.heroLife myHero
            snipeHero enemy = fmap head $ BH.movementsTo state (V.heroPos enemy)
            isSnipable = (<40) . V.heroLife
            closeHeroes = map fst $ filter ((==2) . length . snd) justPaths
            justPaths = foldl extractSuccess [] heroTuples
            extractSuccess xs (_, Nothing) = xs
            extractSuccess xs (hero, Just path) = (hero, path):xs
            heroTuples = zip otherHeroes $ map (BH.movementsTo state . V.heroPos) otherHeroes

      haveTavernMoney = 10 < V.heroGold myHero
      nextToTavern = any (== V.TavernTile) adjacentTiles 
      adjacentTiles = map snd $ BH.adjacentTiles board myPos
      notFullHealth = (90>) $ V.heroLife $ V.stateHero state
      lowHealth = (50>) $ V.heroLife $ V.stateHero state
      earningRateHighest = playerBetterAt V.heroMineCount (\p c -> (p * 10) > (c * 13))
      wealthLargest = playerBetterAt V.heroGold (>)
      playerBetterAt prop comparator = comparator (prop myHero) (prop competition)
         where
            competition = bestOtherHeroBy (comparing prop)
      bestOtherHeroBy comparator = head $ reverse $ sortBy comparator otherHeroes
      otherHeroes = filter (/= myHero) $ V.gameHeroes $ V.stateGame state

      goToNearestTavern = goToClosest state (== V.TavernTile)
      goToNearestMine = goToClosest state isAvailableMine
      isAvailableMine (V.MineTile (Just heroId)) = heroId /= (V.heroId myHero)
      isAvailableMine (V.MineTile Nothing) = True
      isAvailableMine _ = False

      board = V.gameBoard $ V.stateGame state
      myPos = V.heroPos myHero
      myHero = V.stateHero state

goToClosest state matcher = case allPaths of
                               [] -> V.Stay
                               path:_ -> head path 
   where
      allPaths = sortBy pathLength $ catMaybes $ map pathTo $ allMatches
      pathLength = comparing length
      pathTo = BH.movementsTo state
      allMatches = BH.findMatchingTiles state matcher

pathsToItems state items = allPaths
   where
      allPaths = sortBy pathLength $ catMaybes $ map pathTo $ items
      pathLength = comparing length
      pathTo = BH.movementsTo state
      
