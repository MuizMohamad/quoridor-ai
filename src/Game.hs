{-
    Module: Game.

    Functions used in the game loop to change the game state.
-}
module Game where

import Types
import Constants
import Action
import Board 
import Player 
import Data.Maybe
import Debug.Trace



       
{-
    'performAction' and helpers.
-}


-- The current player is the first element in the players list.
currentPlayer :: [Player] -> Player 
currentPlayer = head

-- The previous player is the last element in the players list.
previousPlayer :: [Player] -> Player 
previousPlayer = last

-- The player that just played goes to the back of the list. Used to change turn.
rotatePlayers :: [Player] -> [Player]
rotatePlayers [] = [] 
rotatePlayers (p:ps) = ps ++ [p]

-- A step action is valid if the step is valid and no other player is in the target cell (canMove).
validStepAction :: Game -> Step -> Bool
validStepAction (Game b ps) s = (canMove (currentPlayer ps) ps s) && (validStep b s)

-- Generate all valid steps at a game state.
validSteps :: Game -> [Action]
validSteps g@(Game b ps) = map Move (filter (validStepAction g) steps)
    where  
        steps = let p = currentPlayer ps in (makeSteps (currentCell p) (adjacentCells p) ++ makeSteps (currentCell p) (adjacentCellsTwo p))

-- A wall action is valid if the wall is valid and player has walls remaining.

allPlayerCanWin :: Game -> Bool
allPlayerCanWin game@(Game b ps) = and [(distanceWinPosHelper [currentCell p] b p []) < 100 | p <- ps] 

validWallAction' :: Game -> Wall -> Bool 
validWallAction' (Game b ps) w = (hasWallsLeft (currentPlayer ps)) && (validWall b w)

{-
    *** Extension : Helper function for an extension below ***
    
    When there is no path, the return value of distance would be very large, which will pass the threshold.
    
-}

checkIfVisitedWinPosHelper :: [Cell] -> [Cell] -> Bool
checkIfVisitedWinPosHelper visited winPos = or [ x `elem` winPos | x <- visited] 

distanceWinPosHelper :: [Cell] -> Board -> Player -> [Cell]  -> Int
distanceWinPosHelper checkCell board player visited =
    let reachablesFromCheckCell = concat [reachableCells board c | c  <- checkCell]
        adjCellList = [ x | x <- reachablesFromCheckCell, not (x `elem` visited)]
        visitedWinPos = checkIfVisitedWinPosHelper adjCellList (winningPositions player)
    in if visitedWinPos then 0
       else if adjCellList == [] then 100000 -- Case when there is no path to winning positions.
       else 1 + ( distanceWinPosHelper adjCellList board player (adjCellList ++ visited))
       
{-
    *** Extension : Cannot place wall that completely block any player's path ***
    
    Adding one of the rule in Quoridor at which someone cannot place a wall 
    that blocks all path of a player to a winning positions.
    
    This is done by checking that a distance to winning positions is below the maximum travel distance.
    
-}

validWallAction :: Game -> Wall -> Bool 
validWallAction g@(Game b (p:ps)) w = validWallAction' g w && allCanWin
    where allCanWin = allPlayerCanWin (Game (placeWall b w) (rotatePlayers ((useWall (nextTurn p)):ps)))
  
          
-- Generate all valid walls at a game state.
validWalls :: Game -> [Action]
validWalls g = map Place (filter (validWallAction g) walls)
    where 
        walls = concat [[wallRight c, wallTop c] | c<-[(i, j) | i<-allColumns, j<-allRows]]

-- Generate all valid actions at a game state.
validActions :: Game -> [Action]
validActions g = (validSteps g) ++ (validWalls g)


          
-- Key function. Given a game and an action, checks the validity of the action and applies it to the
-- game, generating a new game.
performAction :: Game -> Action -> Maybe Game
performAction g@(Game b (p:ps)) (Move s)
    | validStepAction g s = 
        Just (Game b (rotatePlayers ((movePlayer (nextTurn p) s):ps)))
    | otherwise = Nothing
performAction g@(Game b (p:ps)) (Place w)
    | validWallAction g w = 
        Just (Game (placeWall b w) (rotatePlayers ((useWall (nextTurn p)):ps)))
    | otherwise = Nothing
