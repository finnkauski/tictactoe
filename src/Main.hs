module Main where

import           Logic
import           Extras
import           Render
import           Data.Array

main :: IO ()
main = gameLoop emptyBoard X

-- scaffolding
emptyBoard :: Board
emptyBoard = listArray ((1, 1), (3, 3)) [ Empty | _ <- [1 .. 9] ]

-- game handling
gameLoop :: Board -> Cell -> IO ()
gameLoop board player = case checkBoard board of
  Just XWins   -> print "X Wins"
  Just OWins   -> print "O Wins"
  Just Draw    -> print "Draw"
  Just Playing -> do
    render board
    ref <- getLine
    gameLoop (takeTurn board player (read ref)) (toggleCell player)
  Nothing -> print "There has been an issue with getting the GameState"

-- TODO: check interact instead of this
-- TODO: the overwritting check isn't done right because it still swaps over the player
-- TODO: should print the board right at the end as well
