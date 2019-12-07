{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module Minesweeper.Types where

import Minesweeper.Utils
import Minesweeper.Colors (Colors)

import Control.Lens
import Data.Array
import Data.List (nub)
import System.Random


data Game = Game { _cursor :: (Int, Int)
                 , _world :: World
                 , _colors :: Colors
                 }

data World = World { _size :: (Int, Int)
                   , _field :: Array (Int, Int) Cell
                   }

data Cell = Cell { _mine :: Bool
                 , _state :: CellState
                 }

data CellState = Default | Marked | Cleared deriving (Eq)

$(makeLenses ''Game)
$(makeLenses ''World)
$(makeLenses ''Cell)


coord :: (Int, Int) -> Lens' World Cell
coord ix = lens get' set'
  where
    get' = (! ix) . view field
    set' world cell = over field (// [(ix, cell)]) world


genWorld :: (Int, Int) -> Int -> IO World
genWorld (sx, sy) nMines = makeWorld (sx, sy) <$> mines
  where
    mines = do
      gen1 <- newStdGen
      gen2 <- newStdGen
      return . take nMines . nub $ zip (randomRs (1, sx) gen1) (randomRs (1, sy) gen2)


makeWorld :: (Int, Int) -> [(Int, Int)] -> World
makeWorld (sx, sy) mines =
  World { _size = (sx, sy)
        , _field = array ((1,1), (sx, sy)) arrayGen
        }
  where
    arrayGen = [((x, y), makeCell $ (x, y) `elem` mines) | x <- [1..sx], y <- [1..sy]]


makeCell :: Bool -> Cell
makeCell mine = Cell { _mine = mine, _state = Default }


makeGame :: World -> Colors -> Game
makeGame world colors = Game { _cursor = (1, 1)
                             , _world = world
                             , _colors = colors
                             }


countMinesAround :: (Int, Int) -> World -> Int
countMinesAround ix world =
  length . filter (\ix -> world ^. coord ix . mine) $ coordinatesAround ix world

coordinatesAround :: (Int, Int) -> World -> [(Int, Int)]
coordinatesAround ix world =
  [ adjacent |
    dx <- [-1..1],
    dy <- [-1..1],
    let adjacent = zipTuplesWith (+) ix (dx, dy),
        adjacent /= ix,
        inRange ((1,1), world^.size) adjacent ]
