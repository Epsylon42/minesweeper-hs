module Minesweeper.Display where

import Minesweeper.Types
import qualified Minesweeper.Colors as C

import Control.Lens
import Control.Monad
import Data.Array ((!))
import UI.NCurses


data CellWithColor = CellWithColor String ColorID


displayWorld :: (Int, Int) -> Game -> Update ()
displayWorld (ox, oy) game =
  let (sx, sy) = game^.world.size
  in forM_ [1..sy] $ \line -> do
       moveCursor (toInteger (oy + line - 1)) (toInteger ox)
       forM_ [1.. sx] $ \col -> do
         if (col, line) == game^.cursor
           then setAttribute AttributeReverse True
           else pure ()
         printCell . showCell (col, line) $ game
         setAttribute AttributeReverse False
       setColor $ C.def $ game^.colors


printCell :: CellWithColor -> Update ()
printCell (CellWithColor s col) = do
  setColor col
  drawString s


showCell :: (Int, Int) -> Game -> CellWithColor
showCell ix game =
  case game^.world.coord ix of
    Cell { _state = Default }
      -> CellWithColor "#" $ C.closed col
    Cell { _state = Marked }
      -> CellWithColor "P" $ C.flag col
    Cell { _state = Cleared, _mine = True }
      -> CellWithColor "X" $ C.mine col
    Cell { _state = Cleared, _mine = False }
      -> case countMinesAround ix $ game^.world of
            0 -> CellWithColor "." $ C.opened col
            x -> CellWithColor (show x) (C.count col ! x)
    where
      col = game^.colors


displayVictory = displayString "You Won!"
displayLoss = displayString "You Lost"


displayString :: String -> Game -> Window -> Curses ()
displayString str game win = do
  updateWindow win $ do
    displayWorld (1, 1) $ game
    moveCursor ((2 +) . toInteger . snd $ game^.world.size) 1
    drawString str
  render

  getEvent win Nothing
  return ()
