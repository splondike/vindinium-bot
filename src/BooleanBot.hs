{-# LANGUAGE OverloadedStrings #-}
module BooleanBot
        ( bot,
          initialBotState
        )
    where

import Data.String(fromString)
import Data.Ord (comparing, Down)
import Data.List (sortBy)
import Data.Maybe (catMaybes, fromJust, isJust)

import System.Random (getStdRandom, randomR)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as ST
import qualified Data.Map.Strict as Map

import qualified BotHelper as BH
import qualified Vindinium as V

type MineTurnovers = Map.Map V.Pos (LastOwner, Int)
type LastOwner = Maybe V.HeroId

type BotState = MineTurnovers

initialBotState = Map.empty

-- | The actual bot
bot :: V.Bot BotState
bot state = updateMineTurnovers state >> ST.get >>= \mineTurnovers -> pickRule [
               ("heal to full", tupleRule (notInHealLoopPosition && nextToTavern && notFullHealth && haveTavernMoney, goToNearestTavern)),
               ("snipe hero", trySnipeHero),
               ("low health", tupleRule (haveTavernMoney && lowHealth, goToNearestTavern)),
               ("i'm winning", tupleRule (earningRateHighest && wealthLargest && haveSomeMoney, goToNearestTavern)),
               ("attack high earning hero", chaseOtherHero),
               ("go to mine", tupleRule (True, goToBestMine mineTurnovers))
            ]
   where
      pickRule rules = do
         let matches = filter (isJust . snd) rules
         let (name, action) = case matches of
                                 [] -> ("no matching rules", V.Stay)
                                 (name, Just action):_ -> (name, action)
         lift $ V.logDebug name
         lift . V.logDebug . fromString $ "threat level: " ++ (show threatLevel)
         return action
      tupleRule (True, action) = Just action
      tupleRule _ = Nothing

      trySnipeHero = case filter isSnipable closeHeroes of
                          [] -> Nothing
                          enemy:_ -> if imHealthy then snipeHero enemy else Nothing
         where
            imHealthy = (>20) $ V.heroLife myHero
            snipeHero enemy = fmap head $ BH.movementsTo state (V.heroPos enemy)
            isSnipable = (<=20) . V.heroLife
            closeHeroes = map fst $ filter ((==2) . length . snd) heroPaths

      chaseOtherHero = if imHealthy then chaseOtherHero' else Nothing
         where
            chaseOtherHero' = case filter goodTarget orderedHeroTuples of
                                 [] -> Nothing
                                 (enemy, path):_ -> Just $ head path

            imHealthy = (>40) $ V.heroLife myHero
            snipeHero enemy = fmap head $ BH.movementsTo state (V.heroPos enemy)
            orderedHeroTuples = reverse $ sortBy (comparing attractiveness) heroPaths

            goodTarget (hero, path) = V.heroMineCount hero >= 5 &&
                                      length path <= 10 &&
                                      (not . any (== V.TavernTile) $ adjacentTiles hero)
            attractiveness (hero, path) = V.heroMineCount hero -
                                          (fromIntegral $ length path `div` 2)
            adjacentTiles hero = map snd $ BH.adjacentTiles board $ V.heroPos hero

      notInHealLoopPosition = case filter isLoopHero adjacentTiles of
                                   [] -> True
                                   heroes -> False
         where
            isLoopHero (V.HeroTile hid) = any (==V.TavernTile) (tilesNextTo hid)
            isLoopHero _ = False
            tilesNextTo hid = map snd $ BH.adjacentTiles board $ V.heroPos $ heroById hid
      haveTavernMoney = 10 < V.heroGold myHero
      nextToTavern = any (== V.TavernTile) adjacentTiles 
      adjacentTiles = map snd $ BH.adjacentTiles board myPos
      notFullHealth = (90>) $ V.heroLife myHero
      lowHealth = ((30 + 2*threatLevel)>) $ V.heroLife myHero
      -- | 0-10 indicating how close other heroes are, 10 is they are closer
      threatLevel = round $ min 10 (lenSum / 2)
         where
            -- 0-40, where 40 means very high threat
            lenSum = fromIntegral $ sum $ map (invert . (min 11) . length . snd) heroPaths
            invert len = 10 - (len - 1)
      earningRateHighest = playerBetterAt V.heroMineCount (\p c -> (p * 10) > (c * 13))
      wealthLargest = playerBetterAt V.heroGold (\p c -> (p*10) > (c*13))
      haveSomeMoney = 100 < V.heroGold myHero
      playerBetterAt prop comparator = comparator (prop myHero) (prop competition)
         where
            competition = bestOtherHeroBy (comparing prop)
      bestOtherHeroBy comparator = head $ reverse $ sortBy comparator otherHeroes
      -- | Paths to all other heroes I can reach
      heroPaths :: [TargetAndPath V.Hero]
      heroPaths = targetsWithPaths state V.heroPos otherHeroes
      otherHeroes = filter (/= myHero) $ V.gameHeroes $ V.stateGame state
      heroById :: V.HeroId -> V.Hero
      heroById hid = head $ filter match $ V.gameHeroes $ V.stateGame state
         where
            match hero = V.heroId hero == hid


      goToNearestTavern = goToClosest state pathCostFunc (== V.TavernTile)
      goToBestMine turnoverMap = case orderedPairs of
                                      [] -> V.Stay
                                      (_, path):_ -> head path
         where
            orderedPairs = sortBy comparator pairs
            comparator = comparing (\(c, p) -> c + (length p) * 2)
            pairs = map (\(a, Just b) -> (a, b)) $ filter (isJust . snd) $ zip turnovers paths
            turnovers = map (extract) allMatches
            extract pos = snd $ Map.findWithDefault (Nothing, 0) pos turnoverMap
            paths = map (BH.movementsTo state) allMatches
            allMatches = BH.findMatchingTiles state isAvailableMine
            
      isAvailableMine (V.MineTile (Just heroId)) = heroId /= (V.heroId myHero)
      isAvailableMine (V.MineTile Nothing) = True
      isAvailableMine _ = False

      pathCostFunc pos = 0
      -- pathCostFunc pos = length $ filter id $ map (adjacentTo pos . V.heroPos) otherHeroes
      adjacentTo (V.Pos x1 y1) (V.Pos x2 y2) = abs (x1 - x2) + abs (y1 - y2) == 1

      board = V.gameBoard $ V.stateGame state
      myPos = V.heroPos myHero
      myHero = V.stateHero state

type TargetAndPath a = (a, [V.Dir])
targetsWithPaths :: V.State -> (a -> V.Pos) -> [a] -> [TargetAndPath a]
targetsWithPaths state posExtractor targets = foldl makeTuple [] targets
   where
      makeTuple xs a = case BH.movementsTo state $ posExtractor a of
                            Nothing -> xs
                            Just path -> (a, path):xs

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

goToClosest state costFunc matcher = case allPaths of
                               [] -> V.Stay
                               path:_ -> head path 
   where
      allPaths = sortBy pathLength $ catMaybes $ map pathTo $ allMatches
      pathLength = comparing length
      pathTo = BH.movementsToWithCost state costFunc
      allMatches = BH.findMatchingTiles state matcher
