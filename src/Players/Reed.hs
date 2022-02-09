{-
    Module: Reed.

    *** PART III (10 pt) ***

    Define a player that uses teh Reed opening and play against it. Is the Reed opening a good 
    opening? Write your answers in Reed.txt.
-}
module Players.Reed where

import Data.Char
import Data.String

import Types
import Action
import Cell 
import Player
import Board
import Game

import Players.Minimax
import Players.Negamax

-- Create a player that starts with the Reed opening. After that, you may use your minimax action or
-- the given action for DumbPlayer. 
-- [Hint 1: Use the variable 'turn' in Player.]
-- [Hint 2: Use 'wallTop' to get the walls you need.]
-- [Hint 3: Don't forget to check that the action is valid using 'validWallAction'.]
reedPlayerAction :: Board -> [Player] -> String -> Int -> Maybe Action
reedPlayerAction b (p:ps) str int 
    | validWallAction (Game b (p:ps)) (wallTop ('c',3)) =  Just (Place (wallTop ('c', 3)))
    | validWallAction (Game b (p:ps)) (wallTop ('f',3)) =  Just (Place (wallTop ('f', 3)))
    | otherwise = minimaxAction b (p:ps) str int

{-
    *** Extension ***
    
    Reed can also use negamax for its next action instead of minimax
    
-}

reedNegamaxPlayerAction :: Board -> [Player] -> String -> Int -> Maybe Action
reedNegamaxPlayerAction b (p:ps) str int 
    | validWallAction (Game b (p:ps)) (wallTop ('c',3)) =  Just (Place (wallTop ('c', 3)))
    | validWallAction (Game b (p:ps)) (wallTop ('f',3)) =  Just (Place (wallTop ('f', 3)))
    | otherwise = negamaxAction b (p:ps) str int

-- We build a Reed player from a name, a starting cell, a number of walls, an array of winning
-- positions and 'commandToAction'.
makeReedPlayer :: String -> Cell -> Int -> [Cell] -> Player
makeReedPlayer n c rws wps = Player {
    name = n,
    turn = 1,
    currentCell = c, 
    remainingWalls = rws,
    winningPositions = wps,
    isHuman = False,
    chooseAction = reedPlayerAction } 

makeReedNegamaxPlayer n c rws wps = Player {
    name = n,
    turn = 1,
    currentCell = c, 
    remainingWalls = rws,
    winningPositions = wps,
    isHuman = False,
    chooseAction = reedNegamaxPlayerAction } 