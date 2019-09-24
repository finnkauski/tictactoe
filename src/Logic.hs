-- | the logic for the tictactoe game

module Logic where

import           Extras
import           Data.Array

-- turn related
takeTurn :: Board -> Cell -> Int -> Board
takeTurn board cell ref = case fmap (checkVacant board) (ref2Coord ref) of
  Just (True , idx) -> board // [(idx, cell)]
  Just (False, _  ) -> board
  Nothing           -> board

checkVacant :: Board -> Coord -> (Bool, Coord)
checkVacant board coord = ((board ! coord) == Empty, coord)

toggleCell :: Cell -> Cell
toggleCell X     = O
toggleCell O     = X
toggleCell Empty = undefined

-- win condition checking
getRelevantIndices :: Board -> [[Cell]]
getRelevantIndices board = cols ++ rows ++ [diag, diag']
 where
  cols  = [ [ board ! (i, j) | i <- [1 .. 3] ] | j <- [1 .. 3] ]
  rows  = [ [ board ! (j, i) | i <- [1 .. 3] ] | j <- [1 .. 3] ]
  diag  = map (board !) (zip [1 .. 3] [1 .. 3])
  diag' = map (board !) (zip [1 .. 3] $ reverse [1 .. 3])

whoWon :: [[Cell]] -> Maybe GameState
whoWon rows = case check of
  [True , False, False] -> Just Draw
  [False, True , False] -> Just XWins
  [False, False, True ] -> Just OWins
  [False, False, False] -> Just Playing
  _                     -> Nothing
 where
  checker player = any (all (== player)) rows -- check if there is a row completed by a given player
  draw  = (all (not . (elem Empty)) rows)
  check = draw : map checker [X, O] -- check which player has completed a row

checkBoard :: Board -> Maybe GameState
checkBoard = whoWon . getRelevantIndices

-- conversion from cell number to coordinates
ref2Coord :: Int -> Maybe Coord
ref2Coord ref = lookup ref mapper
 where
  mapper =
    [ (1, (1, 1))
    , (2, (1, 2))
    , (3, (1, 3))
    , (4, (2, 1))
    , (5, (2, 2))
    , (6, (2, 3))
    , (7, (3, 1))
    , (8, (3, 2))
    , (9, (3, 3))
    ]
