module Bot
        ( bot
        )
    where

import Data.Ord (comparing)
import Data.List (sortBy)
import Data.Maybe (catMaybes)

import System.Random (getStdRandom, randomR)
import Data.Maybe (fromJust)
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

import qualified BotHelper as BH
import qualified Vindinium as V

bot :: V.Bot
bot state = if lowHealth then goToTavern else goToMine
   where
      lowHealth = (50>) $ V.heroLife $ V.stateHero state
      goToTavern = goToClosest state (== V.TavernTile)
      goToMine = goToClosest state isAvailableMine

      isAvailableMine (V.MineTile (Just heroId)) = heroId /= myHeroId
      isAvailableMine (V.MineTile Nothing) = True
      isAvailableMine _ = False
      myHeroId = V.heroId $ V.stateHero state

goToClosest state matcher = return $ case allPaths of
                                   [] -> V.Stay
                                   path:_ -> head path 
   where
      allPaths = sortBy pathLength $ catMaybes $ map pathTo $ allMatches
      pathLength = comparing length
      pathTo = BH.movementsTo state
      allMatches = BH.findMatchingTiles state matcher
