module Main where

import Minesweeper
import Minesweeper.Colors (defaultColors)

import UI.NCurses

main :: IO ()
main = do
  world <- genWorld (10, 10) 10
  runCurses $ do
    colors <- defaultColors
    runGame $ makeGame world colors
