{-# LANGUAGE OverloadedStrings #-}
-- module FuzzyBot
--         ( bot,
--           initialBotState
--         )
--     where
module FuzzyBot where

import Data.Ord (comparing)
import Data.List (sortBy, foldl1)

import Fuzzy ((~==), (~/=), (~||), (~&&), (~=>))
import qualified Fuzzy as F
import qualified Vindinium as V
import qualified BotHelper as BH

import Debug.Trace

initialBotState :: ()
initialBotState = ()

bot state = return . (applyAction state) . (calculateDecision state) $ (calculateFuzzyValues state)

calculateDecision :: V.State -> FuzzyValues -> Action
calculateDecision state fuzzyValues = traceShow decisions action
   where
      decisions = (show topLevelGoal ++ ", " ++ show subordinateGoalSet)
      action = calculateAction state fuzzyValues subordinateGoalSet
      topLevelGoal = (bestOption . extractSet) topLevelRules
      subordinateGoalSet = case topLevelGoal of
                                Just IncreaseIncome -> extractSet increaseIncomeRules
                                Just MaintainLead -> extractSet maintainLeadRules
                                _ -> error "No top level goals?"

      extractSet ruleSet = F.runRuleTree $ ruleSet state fuzzyValues

-- Decisions

data TopLevelGoal = IncreaseIncome | MaintainLead deriving (Eq, Ord, Show)
data SubordinateGoal = HealHero | TakeMines | AttackHero deriving (Eq, Ord, Show)
data Action = GoTowards V.Pos | Stay deriving (Eq, Ord, Show)

topLevelRules state v = F.alternatives [
      imWinning ~=> F.val MaintainLead,
      mapSmall ~|| (F.not imWinning) ~=> F.val IncreaseIncome
   ]
   where
      mapSmall = fvMapSize v ~== Small ~&& fvMineCount v ~== Low
      imWinning = F.fuzzifyBool $ haveSomeMoney && earningRateHighest

      haveSomeMoney = 50 < V.heroGold myHero
      earningRateHighest = playerBetterAt V.heroMineCount (\p c -> (p * 10) > (c * 13))
      playerBetterAt prop comparator = comparator (prop myHero) (prop competition)
         where
            competition = bestOtherHeroBy (comparing prop)
      bestOtherHeroBy comparator = head $ reverse $ sortBy comparator otherHeroes
      otherHeroes = filter (/= myHero) $ V.gameHeroes $ V.stateGame state
      myHero = V.stateHero state

increaseIncomeRules state v = F.alternatives [
      mapSmall ~=> F.val TakeMines,
      F.not mapSmall ~=> notSmallRules
   ]
   where
      notSmallRules = F.alternatives [
            haveTavernMoney ~=> F.alternatives [
               myHealthiness ~== Low ~=> F.val HealHero,
               myHealthiness ~== Medium ~&& nextToTavern ~=> F.val HealHero,
               F.weight 0.2 ~=> F.val HealHero
            ],
            shouldAttack ~=> F.val AttackHero,
            F.weight 0.3 ~=> F.val TakeMines
         ]

      haveTavernMoney = F.fuzzifyBool $ 2 < V.heroGold myHero
      nextToTavern = F.fuzzifyBool $ any (isTavern . snd) adjacentTiles
      mapSmall = fvMapSize v ~== Small ~&& fvMineCount v ~== Low

      shouldAttack = assessAll worthAttacking enemies
      worthAttacking e = (lowHealthEnemyClose e ~|| highIncomeEnemy e)
      lowHealthEnemyClose e = (enemyDistance e) ~== Small ~&& (enemyHealthiness e) ~== Low
      highIncomeEnemy e = (enemyIncome e) ~/= Low ~&& (enemyDistance e) ~== Small
      enemyDistance e = (fvDistanceCalc v) myPos (V.heroPos e)
      enemyIncome e = (fvIncomeCalc v) e
      enemyHealthiness e = fvHealthiness v e

      isHero (V.HeroTile _) = True
      isHero _ = False
      isTavern = (==) V.TavernTile
      assessAll _ [] = 0.0
      assessAll pred items = foldl1 (~||) $ map pred items

      board = V.gameBoard $ V.stateGame state
      myHero = V.stateHero state
      myPos = V.heroPos myHero
      myLife = V.heroLife myHero
      myHealthiness = fvHealthiness v myHero
      adjacentTiles = BH.adjacentTiles board myPos
      adjacentHeroes = map findHero $ filter isHero $ map snd adjacentTiles
         where
            findHero (V.HeroTile hid) = BH.heroById state hid
      enemies = filter ((\hid -> hid /= V.heroId myHero) . V.heroId) allHeroes
      allHeroes = V.gameHeroes . V.stateGame $ state

maintainLeadRules state v = F.alternatives [
      (F.weight 1.0, F.val HealHero)
   ]

calculateAction state v goal = 
   case bestOption $ F.runRuleTree rules of
        Just a -> a
        Nothing -> Stay
   where
      rules = makeRules [
            (HealHero, tavernOptions),
            (TakeMines, mineOptions),
            (AttackHero, attackOptions)
         ]
      makeRules pairs = F.alternatives $ map makeRule pairs
         where
            makeRule (goal', options) = goal ~== goal' ~=> (toAlternative options)
            toAlternative = F.alternatives . concat

      tavernOptions = map tavernRule allTaverns
      mineOptions = map mineRule allMines
      attackOptions = map attackRule enemies

      allTaverns = BH.findMatchingTiles state tavernMatcher
      enemies = filter ((\hid -> hid /= V.heroId myHero) . V.heroId) allHeroes
      allHeroes = V.gameHeroes . V.stateGame $ state
      allMines = BH.findMatchingTiles state mineMatcher

      tavernRule pos = [
            (distanceToMe pos) ~== Small ~=> F.val (GoTowards pos),
            F.weight 0.1 ~=> F.val (GoTowards pos)
         ]
      mineRule pos = [
            myHealthiness ~/= Low ~=> F.alternatives [
                  (distanceToMe pos) ~== Small ~=> F.val (GoTowards pos),
                  (distanceGradientToMe pos) ~=> F.val (GoTowards pos)
               ]
         ]
      attackRule enemy = [
            distanceToEnemy ~== Small ~&& enemyLife ~== Low ~=> F.val (GoTowards enemyPos),
            myHealthiness ~== High ~&& enemyIncome ~/= Low ~&& distanceToEnemy ~/= Large ~&& enemyToTavern ~== Large ~=> F.val (GoTowards enemyPos)
         ]
         where
            enemyToTavern = foldl1 F.union $ map (fvDistanceCalc v enemyPos) allTaverns
            distanceToEnemy = distanceToMe enemyPos
            enemyPos = V.heroPos enemy
            enemyLife = (fvHealthiness v) enemy
            enemyIncome = (fvIncomeCalc v) enemy

      tavernMatcher = (==) V.TavernTile
      mineMatcher (V.MineTile (Just heroId)) = heroId /= myHeroId
      mineMatcher (V.MineTile Nothing) = True
      mineMatcher _ = False
      myHeroId = V.heroId $ V.stateHero state

      myHealthiness = fvHealthiness v myHero
      distanceToMe pos = (fvDistanceCalc v) myPos pos
      distanceGradientToMe pos = case BH.movementsTo state pos of
                                      Nothing -> 0.0
                                      Just path -> 1 - ((fromIntegral . length $ path) / 100.0)
      myHero = V.stateHero state
      myPos = V.heroPos myHero

goToClosest state matcher = 
      case sortBy (comparing (length . snd)) paths of
         [] -> Stay
         (target, _):_ -> GoTowards target
   where
      paths = targetsWithPaths state id allMatches
      allMatches = BH.findMatchingTiles state matcher

type TargetAndPath a = (a, [V.Dir])
targetsWithPaths :: V.State -> (a -> V.Pos) -> [a] -> [TargetAndPath a]
targetsWithPaths state posExtractor targets = foldl makeTuple [] targets
   where
      makeTuple xs a = case BH.movementsTo state $ posExtractor a of
                            Nothing -> xs
                            Just path -> (a, path):xs

bestOption :: F.FuzzySet a -> Maybe a
bestOption set = case sortBy (comparing snd) $ F.iterSet set of
                [] -> Nothing
                items -> Just $ fst $ head $ reverse items

-- Fuzzy value calculations

data FuzzyValues = FuzzyValues {
   fvHealthiness :: V.Hero -> F.FuzzySet LowMedHigh,
   fvMapSize :: F.FuzzySet SmallLarge,
   fvMineCount :: F.FuzzySet LowMedHigh,
   fvIncomeCalc :: V.Hero -> F.FuzzySet LowMedHigh,
   fvDistanceCalc :: V.Pos -> V.Pos -> F.FuzzySet SmallLarge
}
data LowMedHigh = Low | Medium | High deriving (Eq, Ord, Show)
data SmallLarge = Small | Large deriving (Eq, Ord, Show)

calculateFuzzyValues :: V.State -> FuzzyValues
calculateFuzzyValues state =
      FuzzyValues healthiness mapSize mineCount incomeCalc distanceCalc
   where
      healthiness h = (flip F.fuzzify) health [
            (Low, F.down 20 40),
            (Medium, F.trapezoid 30 40 60 80),
            (High, F.up 70 90)
         ]
         where
            health = fromIntegral $ V.heroLife h

      mapSize = (flip F.fuzzify) mapSize' [
            (Small, F.down 10 15),
            (Large, F.up 15 20)
         ]
      mapSize' = fromIntegral $ V.boardSize board

      mineCount = (flip F.fuzzify) mineCount' [
            (Low, F.down 0 8),
            (Medium, F.trapezoid 7 10 15 20),
            (High, F.up 18 20)
         ]
      mineCount' = fromIntegral $ length $ BH.findMatchingTiles state isMine
      isMine (V.MineTile _) = True
      isMine _ = False

      incomeCalc = incomeCalc' . fromIntegral . V.heroMineCount
         where
            incomeCalc' = F.fuzzify [
                  (Low, F.down 4 6),
                  (Medium, F.trapezoid 4 6 8 10),
                  (High, F.up 8 10)
               ]

      distanceCalc pos1 pos2 = (flip F.fuzzify) distance [
               (Small, F.down 0 7),
               (Large, F.up 6 10)
            ]
         where
            distance = case BH.calcAstar board pos1 pos2 (const 0) of
                            Nothing -> 100
                            Just p -> fromIntegral $ length p

      board = V.gameBoard $ V.stateGame state
      myHero = V.stateHero state

-- Application of decision

applyAction state Stay = V.Stay
applyAction state (GoTowards pos) = case BH.movementsTo state pos of
                                         Nothing -> V.Stay
                                         Just [] -> V.Stay
                                         Just (d:_) -> d
