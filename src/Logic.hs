-- | the logic for the tictactoe game

module Logic where

import           Extras
import           Render
import           Data.Array

-- turn related
takeTurn :: State -> IO State
takeTurn (State board turn gamestate) = do
  render board
  ref <- fmap read getLine
  return $ case fmap (checkVacant board) (ref2Coord ref) of
    Just (True, idx) ->
      State (board // [(idx, turn)]) (toggleCell turn) Playing
    Just (False, _) -> State board turn gamestate
    Nothing         -> State board turn gamestate


checkVacant :: Board -> Coord -> (Bool, Coord)
checkVacant board coord = ((board ! coord) == Empty, coord)

toggleCell :: Cell -> Cell
toggleCell X     = O
toggleCell O     = X
toggleCell Empty = undefined

-- win condition checking
getRelevantIndices :: State -> [[Cell]]
getRelevantIndices (State board _ _) = cols ++ rows ++ [diag, diag']
 where
  cols  = [ [ board ! (i, j) | i <- [1 .. 3] ] | j <- [1 .. 3] ]
  rows  = [ [ board ! (j, i) | i <- [1 .. 3] ] | j <- [1 .. 3] ]
  diag  = map (board !) (zip [1 .. 3] [1 .. 3])
  diag' = map (board !) (zip [1 .. 3] $ reverse [1 .. 3])

whoWon :: [[Cell]] -> GameState
whoWon rows = case check of
  [True , False, False] -> Draw
  [False, True , False] -> XWins
  [False, False, True ] -> OWins
  [False, False, False] -> Playing
  _                     -> error "Error in whoWon"
 where
  checker player = any (all (== player)) rows -- check if there is a row completed by a given player
  draw  = (all (not . (elem Empty)) rows)
  check = draw : map checker [X, O] -- check which player has completed a row

checkState :: State -> GameState
checkState = whoWon . getRelevantIndices

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
