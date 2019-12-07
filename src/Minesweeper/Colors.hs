module Minesweeper.Colors
  ( Colors
  , closed
  , opened
  , mine
  , flag
  , count
  , def
  , defaultColors
  ) where

import Data.Array
import UI.NCurses

data Colors = Colors { closed :: ColorID
                     , opened :: ColorID
                     , mine :: ColorID
                     , flag :: ColorID
                     , count :: Array Int ColorID
                     , def :: ColorID
                     }

cid fg n = newColorID fg ColorDefault n

defaultColors :: Curses Colors
defaultColors = do
  count <- array (1, 8) <$> mapM makeArray (zip (repeat ColorGreen) [1..8])
  closed <- cid ColorWhite 9
  opened <- cid ColorDefault 10
  mine <- cid ColorRed 11
  flag <- cid ColorBlue 12
  def <- cid ColorDefault 13
  return Colors { closed = closed
                , opened = opened
                , mine = mine
                , flag = flag
                , count = count
                , def = def
                }
  where
    makeArray (color, n) = (\id -> (fromInteger n, id)) <$> cid color n
