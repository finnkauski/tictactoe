-- | Rendering for the grid

module Render where

import           Extras
import           Data.Array
import           Data.List

render :: Board -> IO ()
render board = mapM_ putStrLn scanlines
 where
  stringified = fmap cell2String board
  rows        = [ [ stringified ! (j, i) | i <- [1 .. 3] ] | j <- [1 .. 3] ]
  scanlines   = map unwords rows


cell2String :: Cell -> String
cell2String cell = case cell of
  X     -> "[ X ]"
  O     -> "[ O ]"
  Empty -> "[ * ]"
