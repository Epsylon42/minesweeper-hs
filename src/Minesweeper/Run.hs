module Minesweeper.Run (runGame) where

import Minesweeper.Types
import Minesweeper.Utils
import Minesweeper.Display


import Control.Lens
import Data.Array (inRange)
import Data.Tuple (swap)
import UI.NCurses


runGame :: Game -> Curses ()
runGame game = do
  setEcho False
  setCursorMode CursorInvisible
  stepGame game =<< defaultWindow


stepGame :: Game -> Window -> Curses ()
stepGame game win = do
  let Game { _world = world, _cursor = cursor } = game

  updateWindow win $ displayWorld (1, 1) game
  render

  input <- getInput <$> getEvent win Nothing
  processInput input game win


processInput :: Input -> Game -> Window -> Curses ()
processInput input _game win =
  let cursorPos = _game^.cursor
      nextGame = case input of
                   Mark -> over (world . coord cursorPos . state) mark
                   Open -> over world (openCell cursorPos)
                   Move delta -> over cursor (moveCursor' delta)
                   _ -> id
      game = nextGame _game
  in
    case input of
      Exit -> return ()
      _ -> case determineGameResult $ game^.world of
             Nothing -> stepGame game win
             Just Victory -> displayVictory game win
             Just Loss -> displayLoss game win
  where
    mark Default = Marked
    mark Marked = Default
    mark Cleared = Cleared

    moveCursor' (dx, dy) cursor =
      let (x, y) = cursor
          (sx, sy) = _game^.world.size
      in ((x+dx) `clamp` (1,sx) , (y+dy) `clamp` (1,sy))

    x `clamp` (lower, upper) = if x < lower then lower else if x > upper then upper else x


data GameResult = Victory | Loss

determineGameResult :: World -> Maybe GameResult
determineGameResult world =
  let cells = toListOf (field . traverse) world
  in
    if all clearedIfEmpty cells
    then Just Victory
    else if any mineCleared cells
    then Just Loss
    else Nothing
  where
    clearedIfEmpty cell = if not $ cell^.mine then cell^.state == Cleared else True
    mineCleared cell = cell^.state == Cleared && cell^.mine


openCell :: (Int, Int) -> World -> World
openCell ix world =
  applyIf
    (clearAround ix) (set (coord ix . state) Cleared world)
    (world ^. coord ix . state /= Cleared && countMinesAround ix world == 0)
  where
    clearAround ix world =
      foldl (flip openCell) world $ coordinatesAround ix world


data Input = Mark
           | Open
           | Move (Int, Int)
           | Exit
           | None

getInput :: Maybe Event -> Input
getInput event =
  case event of
    Just x -> case x of
                EventCharacter 'q' -> Exit
                EventCharacter 'm' -> Mark
                EventCharacter ' ' -> Open
                EventCharacter 'w' -> Move (0, -1)
                EventCharacter 's' -> Move (0, 1)
                EventCharacter 'a' -> Move (-1, 0)
                EventCharacter 'd' -> Move (1, 0)
                _ -> None
    Nothing -> None
