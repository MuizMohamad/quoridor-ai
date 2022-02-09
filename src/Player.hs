{-
    Module: Player.

    A few utility functions to obtain and update player data.
-}
module Player where 

import Types
import Constants
import Cell
import Action
import Board 

import Debug.Trace

{- 
    Related to 'currentCell'.
-}

-- Given a list of players and a cell, returns the player in that cell if any or 'Nothing' 
-- otherwise.
playerInCell :: [Player] -> Cell -> Maybe Player 
playerInCell [] _ = Nothing 
playerInCell (p:ps) c' 
    | currentCell p == c' = Just p 
    | otherwise = playerInCell ps c'

-- Similar to the one above but returns a boolean.
cellFree :: [Player] -> Cell -> Bool 
cellFree [] c = True
cellFree (p:ps) c = (currentCell p /= c) && cellFree ps c

-- Given a player, returns the cells adjacent to it.
adjacentCells :: Player -> [Cell]
adjacentCells p = cellsAroundInBoard (currentCell p)

-- Extension : Helper function for implementation to jump over players.
adjacentCellsTwo :: Player -> [Cell]
adjacentCellsTwo p = cellsAroundInBoardTwo (currentCell p)

{-
    Useful checks.
-}

-- The player has won if it is in one of the winning positions specified when it was created.
hasWon :: Player -> Bool
hasWon p = (currentCell p) `elem` (winningPositions p)

-- Check if the player has got any walls left.
hasWallsLeft :: Player -> Bool 
hasWallsLeft p = remainingWalls p > 0

-- The player can move if the target cell is free.

{-
    Extension : The player can jump over other player to the behind the player.
    
-}
canMove :: Player -> [Player] -> Step -> Bool 
canMove p ps (cs, ce)   
    | (isAdjacent cs ce) = inCurrentCell && targetCellFree  -- Basic step
    | (isTwoAdjacent cs ce) = inCurrentCell && middleCellNotFree -- Jump over step
    | otherwise = False
    where inCurrentCell = (currentCell p == cs)
          targetCellFree = (cellFree ps ce)
          
          middleCell = head [ c | c <- cellsAroundInBoard ce , c `elem` (cellsAroundInBoard cs) ] -- Cell 'in front' of player to be jumped over
          middleCellNotFree = not (cellFree ps middleCell)
    

{-
    Updating the player.
-}

-- Adds one to Player variable turn.
nextTurn :: Player -> Player 
nextTurn p = p { turn = (turn p) + 1 }

-- Update current cell.
movePlayer :: Player -> Step -> Player 
movePlayer p (_, c') = p { currentCell = c' } 

-- Subtract one from the number of walls remaining.
useWall :: Player -> Player 
useWall p = p { remainingWalls = (remainingWalls p) - 1 }
