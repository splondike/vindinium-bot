module BotHelper where

import Data.Maybe (catMaybes)

import qualified Data.HashSet as H
import qualified Data.Graph.AStar as A

import qualified Vindinium as V

-- | Find the position of all tiles matching the predicate
findMatchingTiles :: V.State -> (V.Tile -> Bool) -> [V.Pos]
findMatchingTiles state matcher = findMatchingTiles' (gameBoard state) matcher

findMatchingTiles' :: V.Board -> (V.Tile -> Bool) -> [V.Pos]
findMatchingTiles' board matcher = positions
   where
      positions = map (indexToPos . snd) $ filter matcher' tuples
      indexToPos index = V.Pos (index `mod` boardSize) (index `div` boardSize)
      matcher' = matcher . fst
      tuples = zip boardTiles [0..]

      boardSize = V.boardSize board
      boardTiles = V.boardTiles board

-- | Finds the shortest way of reaching the target position if such a way exists
movementsTo :: V.State -> V.Pos -> Maybe [V.Dir]
movementsTo state target = fmap mapToDir $ calcAstar board startPos target
   where
      mapToDir positions = snd $ foldl f (startPos, []) positions
      f (currPos, moves) nextPos = (nextPos, moves ++ [calcDir currPos nextPos])
      calcDir pos1@(V.Pos currX currY) pos2
         | pos2 == V.Pos (currX - 1) currY = V.West
         | pos2 == V.Pos (currX + 1) currY = V.East
         | pos2 == V.Pos currX (currY - 1) = V.North
         | pos2 == V.Pos currX (currY + 1) = V.South
         | otherwise = V.Stay -- Something went wrong...

      (V.Pos startX startY) = startPos
      startPos = V.heroPos . V.stateHero $ state
      board = gameBoard state

calcAstar :: V.Board -> V.Pos -> V.Pos -> Maybe [V.Pos]
calcAstar board startPos target = maybePosPath
   where
      maybePosPath = A.aStar graph (\_ _ -> 1) costFunc goalFunc startPos
      graph (V.Pos x y) = H.fromList $ filter validPos $ map applyOffset allowedOffsets
         where
            applyOffset (dx, dy) = V.Pos (x + dx) (y + dy)
      allowedOffsets = [(-1, 0), (1, 0), (0, -1), (0, 1)]
      validPos pos = if pos == target then True else isPassable pos
      isPassable pos = Just V.FreeTile == tileAt board pos
      costFunc (V.Pos x y) = (abs $ startX - x) + (abs $ startY - y)
      goalFunc = (==) target
      (V.Pos startX startY) = startPos

-- | Gets all adjacent tiles to the given position, whether or not they're
-- passable. Will filter out positions off the map.
adjacentTiles :: V.Board -> V.Pos -> [(V.Pos, V.Tile)]
adjacentTiles board (V.Pos x y) = catMaybes maybeResults 
   where
      maybeResults = map posToMaybeTuple adjacentPos
      posToMaybeTuple pos = do
         tile <- tileAt board pos
         return (pos, tile)
      adjacentPos = map applyOffset allowedOffsets
      applyOffset (dx, dy) = V.Pos (x + dx) (y + dy)
      allowedOffsets = [(-1, 0), (1, 0), (0, -1), (0, 1)]

inBoard :: V.Board -> V.Pos -> Bool
inBoard b (V.Pos x y) =
    let s = V.boardSize b
    in x >= 0 && x < s && y >= 0 && y < s

tileAt :: V.Board -> V.Pos -> Maybe V.Tile
tileAt b p@(V.Pos x y) =
    if inBoard b p
        then Just $ V.boardTiles b !! idx
        else Nothing
  where
    idx = y * V.boardSize b + x

gameBoard :: V.State -> V.Board
gameBoard = V.gameBoard . V.stateGame 
