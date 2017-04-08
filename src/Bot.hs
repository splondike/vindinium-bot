{-# LANGUAGE OverloadedStrings #-}
module Bot
        ( bot,
          initialBotState
        )
    where

import Data.Ord (comparing)
import Data.List (sortBy)
import Data.Maybe (catMaybes, fromJust, isJust)

import System.Random (getStdRandom, randomR)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as ST
import qualified Data.Map.Strict as Map

import qualified BotHelper as BH
import qualified Vindinium as V

-- | Some quick global state
type MineTurnovers = Map.Map V.Pos (LastOwner, Int)
type LastOwner = Maybe V.HeroId

type BotState = MineTurnovers

initialBotState = Map.empty

-- | The actual bot
bot :: V.Bot BotState
bot state = updateMineTurnovers state >> ST.get >>= \mineTurnovers -> pickRule [
               ("heal to full", tupleRule (notInHealLoopPosition && nextToTavern && notFullHealth && haveTavernMoney, goToNearestTavern)),
               ("snipe hero", trySnipeHero),
               ("low health", tupleRule (lowHealth && haveTavernMoney, goToNearestTavern)),
               ("i'm winning", tupleRule (earningRateHighest && wealthLargest, goToNearestTavern)),
               ("go to mine", tupleRule (True, goToBestMine mineTurnovers))
            ]
   where
      pickRule rules = do
         let matches = filter (isJust . snd) rules
         let (name, action) = case matches of
                                 [] -> ("no matching rules", V.Stay)
                                 (name, Just action):_ -> (name, action)
         lift $ V.logDebug name
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

      notInHealLoopPosition = case filter isHero (BH.adjacentTiles board myPos) of
                                   [] -> True
                                   heroes -> not $ any touchingTavern heroes
         where
            isHero (_, V.HeroTile _) = True
            isHero _ = False
            touchingTavern (heroPos, _) = any (\(_, t) -> t == V.TavernTile) $ BH.adjacentTiles board heroPos
      haveTavernMoney = 10 < V.heroGold myHero
      nextToTavern = any (== V.TavernTile) adjacentTiles 
      adjacentTiles = map snd $ BH.adjacentTiles board myPos
      notFullHealth = (90>) $ V.heroLife myHero
      lowHealth = (50>) $ V.heroLife myHero
      earningRateHighest = playerBetterAt V.heroMineCount (\p c -> (p * 10) > (c * 13))
      wealthLargest = playerBetterAt V.heroGold (>)
      playerBetterAt prop comparator = comparator (prop myHero) (prop competition)
         where
            competition = bestOtherHeroBy (comparing prop)
      bestOtherHeroBy comparator = head $ reverse $ sortBy comparator otherHeroes
      otherHeroes = filter (/= myHero) $ V.gameHeroes $ V.stateGame state

      goToNearestTavern = goToClosest state (== V.TavernTile)
      goToBestMine turnoverMap = case orderedPairs of
                                      [] -> V.Stay
                                      (_, path):_ -> head path
         where
            orderedPairs = sortBy comparator pairs
            comparator = comparing (\(c, p) -> c + (length p) * 5)
            pairs = map (\(a, Just b) -> (a, b)) $ filter (isJust . snd) $ zip turnovers paths
            turnovers = map (extract) allMatches
            extract pos = snd $ Map.findWithDefault (Nothing, 0) pos turnoverMap
            paths = map (BH.movementsTo state) allMatches
            allMatches = BH.findMatchingTiles state isAvailableMine
            
      isAvailableMine (V.MineTile (Just heroId)) = heroId /= (V.heroId myHero)
      isAvailableMine (V.MineTile Nothing) = True
      isAvailableMine _ = False

      board = V.gameBoard $ V.stateGame state
      myPos = V.heroPos myHero
      myHero = V.stateHero state

updateMineTurnovers :: V.State -> ST.StateT BotState V.Vindinium ()
updateMineTurnovers state = ST.modify update
   where
      update map = foldl upsert map minePosPairs
      upsert map (pos, V.MineTile maybeId) = Map.alter upsert' pos map
         where
            upsert' Nothing = Just (maybeId, 1)
            upsert' prev@(Just (prevId, prevCount))
               | prevId == maybeId = prev
               | otherwise = Just (maybeId, prevCount + 1)

      minePosPairs = map makeTuple $ BH.findMatchingTiles state isMine
      makeTuple pos = (pos, fromJust $ BH.tileAt board pos)
      isMine (V.MineTile _) = True
      isMine _ = False
      board = V.gameBoard $ V.stateGame state

goToClosest state matcher = case allPaths of
                               [] -> V.Stay
                               path:_ -> head path 
   where
      allPaths = sortBy pathLength $ catMaybes $ map pathTo $ allMatches
      pathLength = comparing length
      pathTo = BH.movementsTo state
      allMatches = BH.findMatchingTiles state matcher
