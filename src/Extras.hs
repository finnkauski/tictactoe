-- | extra bits and data structures for the game

module Extras where

import           Data.Array

-- Board type synonym for brevity
type Board = Array (Int, Int) Cell

-- Cell state, either player X takes it, player O takes it or empty
data Cell = X | O | Empty deriving (Eq, Show)

-- Move type
type Coord = (Int, Int)

-- Game state
data GameState = Playing | Draw | XWins | OWins deriving (Show)

data State = State Board Cell GameState
