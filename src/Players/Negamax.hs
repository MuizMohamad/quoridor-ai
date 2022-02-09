{-
    Module: Negamax.
   
-}

module Players.Negamax where 

import Data.Maybe
import Data.Graph
import Data.Ord
import Data.Tree
import Data.List
import Data.Array

import Types
import Constants
import Cell
import Action
import Board 
import Player
import Game
import Players.Dumb (dumbAction)
import Players.Minimax  -- most of helper function are written in Minimax module
import Print

import Debug.Trace

{- 
    *** Extension - Negamax Player which uses negamax algorithm ***
    The utility function is from the perspective of the current player in the Game state,
    therefore negamax, which is a variation might be good to be used here. 
    
    Negamax is simply instead of minimizing and maximizing, we only maximizing because I choose to write the utility function 
    from perspective of the currentPlayer, not from the Max player.
    
-}

multiplyResult :: Int -> Result -> Result
multiplyResult i (Result val x) = Result (i*val) x

negamaxFromTreeMax :: [Action] -> Int -> EvalTree -> Result
negamaxFromTreeMax actionList color (StateTree value []) = multiplyResult (color) $ Result (value) actionList
negamaxFromTreeMax actionList color (StateTree value childList) = {-trace ("Same : " ++ show (length sameMax))-} chosenMaximum
    where explored = [ multiplyResult (-1) (negamaxFromTreeMax (actionList ++ [chosenAction]) (-color) childTree) | (chosenAction, childTree) <- childList ]
          chosenMaximum = maximum explored
          sameMax = [ x | x <- explored , x == chosenMaximum]
    
negamaxFromTree :: EvalTree -> Action
negamaxFromTree evalTree@(StateTree value childList) = head chosenActionList
    where Result value chosenActionList = negamaxFromTreeMax [] 1 evalTree

{- 
    Alpha beta pruning for Negamax algorithm 
-}

negamaxABFromTreeMax :: [Action] -> Result -> Result -> Result -> Int -> EvalTree -> Result
negamaxABFromTreeMax actionList value alpha beta color (StateTree v []) = multiplyResult color (Result v actionList)
negamaxABFromTreeMax actionList value alpha beta color (StateTree v childList@(c:cs)) 
    | (alpha > beta) = value
    | length childList == 1 = updatedValue
    | otherwise =  negamaxABFromTreeMax actionList updatedValue (max updatedValue alpha) beta color (StateTree v (cs))
    where (action,child) = c
          negAlpha = multiplyResult (-1) alpha
          negBeta = multiplyResult (-1) beta
          value2 = negamaxABFromTreeMax (actionList ++ [action]) (Result (-10000) actionList) negBeta negAlpha (-color) child
          negValue2 = multiplyResult (-1) value2
          updatedValue = max value negValue2


negamaxABFromTree :: EvalTree -> Action
negamaxABFromTree evalTree = trace ("Chosen value : " ++ show value ++ "\nAction list : " ++ show chosenActionList) $ head chosenActionList
   where Result value chosenActionList = negamaxABFromTreeMax [] (Result (-10000) []) (Result (-10000) []) (Result (10000) []) 1 evalTree

{-
    Putting everything together.
-}

-- Given depth for pruning (should be even).
depthNegamax :: Int 
depthNegamax = 2

-- Given breadth for pruning.
breadthNegamax :: Int 
breadthNegamax = 20

-- Function that combines all the different parts implemented in Part I.
negamax :: Game -> Action
negamax =
      negamaxABFromTree -- or 'minimaxABFromTree'
    . pruneBreadth breadthNegamax
    . highFirst
    . evalTree
    . pruneDepth depthNegamax
    . generateGameTree 

-- Given a game state, calls negamax and returns an action.
negamaxAction :: Board -> [Player] -> String -> Int -> Maybe Action
negamaxAction b ps _ r = let g = Game b ps in negamaxAction' g (negamax g)
    where 
        -- Goes through the list of actions until it finds a valid one. 
        negamaxAction' :: Game -> Action -> Maybe Action
        negamaxAction' g' (Move s)
            | validStepAction g' s = Just (Move s)
            | otherwise = error "Minimax chose an invalid action."
        negamaxAction' g' (Place w)
            | validWallAction g' w = Just (Place w)
            | otherwise = error "Minimax chose an invalid action."

-- Make negamaxPlayer in the usual way using 'negamaxAction'.
makeNegamaxPlayer :: String -> Cell -> Int -> [Cell] -> Player
makeNegamaxPlayer n c rws wps = Player {
    name = n,
    turn = 1,
    currentCell = c, 
    remainingWalls = rws,
    winningPositions = wps,
    isHuman = False,
    chooseAction = negamaxAction }