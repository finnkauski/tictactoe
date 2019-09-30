module Main where

import           Logic
import           Extras
import           Data.Array

main :: IO ()
main = gameLoop initialState

-- scaffolding
emptyBoard :: Board
emptyBoard = listArray ((1, 1), (3, 3)) [ Empty | _ <- [1 .. 9] ]

initialState :: State
initialState = State emptyBoard X Playing

-- game handling
gameLoop :: State -> IO ()
gameLoop state = case checkState state of
  XWins   -> print "X Wins"
  OWins   -> print "O Wins"
  Draw    -> print "Draw"
  Playing -> do
    newState <- takeTurn state
    gameLoop newState

-- TODO: check interact instead of this
-- TODO: the overwritting check isn't done right because it still swaps over the player
-- TODO: should print the board right at the end as well
