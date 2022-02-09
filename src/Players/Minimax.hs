{-
    Module: Minimax.

    *** PART I (60pt) and PART II (10pt) *** 
-}
module Players.Minimax where 

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
import Print

import Debug.Trace


{- 
    *** Most of the extension are marked with comments that have Extension in various files ***
    
    Searching for "Extension" might be a good way to find the changes.
    
-}


{-
    StateTree util.
-}

-- Map a function through the nodes of the tree.
mapStateTree :: (v -> w) -> StateTree v a -> StateTree w a
mapStateTree f (StateTree x ts) = StateTree (f x) [(a, mapStateTree f t) | (a, t)<-ts]

-- Calculate the depth of the tree (used to test pruneDepth).
stateTreeDepth :: StateTree v a -> Int 
stateTreeDepth (StateTree _ []) = 0
stateTreeDepth (StateTree _ ts) = 1 + (maximum (map (stateTreeDepth . snd) ts))

-- Calculate the breadth of the tree (used to test pruneBreadth).
stateTreeBreadth :: StateTree v a -> Int
stateTreeBreadth (StateTree _ []) = 0
stateTreeBreadth (StateTree _ ts) = max (length ts) (maximum (map (stateTreeBreadth . snd) ts))

{-
    Result util.
-}

-- Negating the result is simply negating the score. You may ignore this although it may be useful
-- to implement the minimax algorithm.
negResult :: Result -> Result
negResult (Result x as) = Result (-x) as

{- 
    *** Part I.a (10pt) ***

    First, we will generate a tree containing all the possible game states.
-}

-- Given a game, return a tree that encodes all the possible future game states.
-- [Hint: Use 'validActions' and 'performAction'.]
-- [Note: To speed things up, you may want to, at this stage, heuristically select which actions are 
--  more relevant. In particular, you probably don't want to consider every single possible wall.]

generateGameTree ::  Game -> GameTree
generateGameTree g@(Game board playerList) 
    | actions == [] = StateTree g []
    | playerHasWon = StateTree g [] -- when a player has won, the gameTree ends (for that child node)
    | otherwise = StateTree g [(a, generateGameTree (fromJust (performAction g a))) | a <- actions, isJust (performAction g a) ]
      where actions = validActions g
            playerHasWon = or [hasWon player | player <- playerList]

{-
    *** PART I.b (5pt) ***

    Re-order the tree so that when traversed by the minimax algorithm, when it traverses the 
    branches at each node, finds either the higher scores or the lower scores first, depending on
    the depth of the tree.
-}

-- Higher scoring nodes go first.

compareHelper :: (Ord b) => (a,StateTree b c) -> (a,StateTree b c) -> Ordering
compareHelper ( _ , StateTree x1 x2) ( _ , StateTree y1 y2)
    | x1 < y1 = GT
    | y1 < x1 = LT
    | otherwise = EQ

-- high first then low first
highLowFirst :: (Ord v) => StateTree v a -> StateTree v a
highLowFirst (StateTree x []) = StateTree x []
highLowFirst (StateTree x xs) = StateTree x (sortBy compareHelper [( a, lowHighFirst y) | (a,y) <- xs ])

lowHighFirst :: (Ord v) => StateTree v a -> StateTree v a
lowHighFirst (StateTree x []) = StateTree x []
lowHighFirst (StateTree x xs) = StateTree x (reverse (sortBy compareHelper [( a, highLowFirst y) | (a,y) <- xs ]))

-- Strictly high first
highFirst :: (Ord v) => StateTree v a -> StateTree v a
highFirst (StateTree x []) = StateTree x []
highFirst (StateTree x xs) = StateTree x (sortBy compareHelper [( a, highFirst y) | (a,y) <- xs ])

{-
    *** Part I.c (5pt) ***

    We don't want to look at all possible future game states as that would consume too much time and
    memory. Instead, we will only look a given number of steps into the future. Formally, the future
    game states are encoded in a tree, so we need a function that reduces the depth of a tree.
-}

-- Given a depth and a tree, return the same tree but cutting off the branches when the depth is 
-- exceeded. 
-- [Hint: You may want to use guards and recursion.]
pruneDepth :: Int -> StateTree v a -> StateTree v a
pruneDepth depth (StateTree x xs) 
    | depth == 0 = StateTree x []
    | otherwise = StateTree x [( a, (pruneDepth (depth-1) y) ) | (a,y) <- xs ]

{-
    *** Part I.d (5pt) ***

    Similarly, we can also make our tree smaller by not considering all the possible game states at
    a given point. We need a function that reduces the breadth (or width) of a tree.
-}

-- Given a breadth (Int n) and a tree, return the same tree but only keeping the first n branches at
-- every node. 
-- [Hint: Use 'take'.]
pruneBreadth :: Int -> StateTree v a -> StateTree v a
pruneBreadth breadth (StateTree x xs) = StateTree x (take breadth [ (a, pruneBreadth breadth y) | (a,y) <- xs])

{-
    *** Part I.e (15pt) ***

    A crucial part of the minimax algorithm is defining a good utility function. It should measure
    how good a game position is for the current player. In our case, a game state should be better
    than another one if the player is closer to its winning positions.
-}

-- Assign a value to each game (from the point of view of the current player).
-- [Hint 1: You may want to calculate the distance between the player's current cell and its winning
--  positions.]
-- [Hint 2: One way would be to use 'reachableCells' repeatedly.]

-- Calculate the distance of player to a cell

checkIfVisitedWinPos :: [Cell] -> [Cell] -> Bool
checkIfVisitedWinPos visited winPos = or [ x `elem` winPos | x <- visited] 

manhattanDistance :: Player -> Int
manhattanDistance p = abs (winningRow-currentRow)
    where (x , currentRow) = currentCell p
          (a , winningRow) = head (winningPositions p)

distanceWinPos :: [Cell] -> Board -> Player -> [Cell] -> Int
distanceWinPos checkCell board player visited =
    let --reachablesFromCheckCell = reachableCells board (head checkCell)
        reachablesFromCheckCell = concat [reachableCells board c | c  <- checkCell]
        adjCellList = [ x | x <- reachablesFromCheckCell, not (x `elem` visited)]
        visitedWinPos = checkIfVisitedWinPos adjCellList (winningPositions player)
    in if visitedWinPos then 0
       else if adjCellList == [] then 100
       else 1 + ( distanceWinPos adjCellList board player (adjCellList ++ visited) )

{-

    Extension : Added a few more features to the utility evaluation score 
    
    Added features:
    1. Current player distance to winning positions (Positive weight
    2. Opponent player distance to winning positions (Negative weight because the shorter the distance the lower the evaluation score)
    3. Current player manhattan distance to winning positions (Positive weight)
    4. Opponent player manhattan distance to winning positions (Negative weight because the shorter the distance the lower the evaluation score)
    
    However, unfortunately adding the features makes the test failed, except for the first one.
    
-}
utility :: Game -> Int 
utility (Game b p) = curDisFeature -- - oppDisFeature -- + curManFeature - oppManFeature
    where curPlayer = currentPlayer p
          curCell = currentCell curPlayer
          oppPlayer = p!!1
          oppCell = currentCell oppPlayer
          
          curShortestDistance = distanceWinPos [curCell] b curPlayer [curCell]
          --oppShortestDistance = distanceWinPos [oppCell] b oppPlayer [oppCell]
          
          maxDistance = (boardSize * boardSize)
          
          curDisFeature = maxDistance - curShortestDistance 
          --oppDisFeature = maxDistance - oppShortestDistance
          
          --curManDist = manhattanDistance (curPlayer)
          --oppManDist = manhattanDistance (oppPlayer)
          
          --curManFeature = maxDistance - curManDist
          --oppManFeature = maxDistance - oppManDist
          
          
       

-- Lifting the utility function to work on trees.
evalTree :: GameTree -> EvalTree 
evalTree = mapStateTree utility 

{-
    *** Part I.f (20pt) ***

    Finally, we ask you to implement the minimax algorithm. Given an evaluation tree, it should 
    return the a high scoring action (according to the minimax algorithm).
-}

-- Given an evaluation tree (it stores a score in the node and each branch is labelled with the 
-- action that leads to the next child) return a list of actions
-- [Hint 1: Use a helper function to keep track of the highest and lowest scores.]
-- [Hint 2: Use the 'Result' datatype.]

minimaxFromTreeMin :: [Action] -> EvalTree -> Result
minimaxFromTreeMin actionList (StateTree value []) = Result value actionList
minimaxFromTreeMin actionList (StateTree value childList) = chosenMinimum
    where resultsList = [ (minimaxFromTreeMax (actionList ++ [chosenAction]) childTree) | (chosenAction, childTree) <- childList ]
          chosenMinimum = minimum resultsList

minimaxFromTreeMax :: [Action] -> EvalTree -> Result
minimaxFromTreeMax actionList (StateTree value []) = {-trace ("Depth : " ++ show depth ++ " Value : " ++ show value) $-} Result value actionList
minimaxFromTreeMax actionList (StateTree value childList) = {-trace ("Depth : " ++ show depth ++ " Value : " ++ show value ++ " Chosen value max : " ++ show valMax)-} chosenMaximum
    where chosenMaximum@(Result valMax actList) = maximum [ (minimaxFromTreeMin (actionList ++ [chosenAction]) childTree) | (chosenAction, childTree) <- childList ]


minimaxFromTree :: EvalTree -> Action
minimaxFromTree evalTree@(StateTree value childList) = trace ("Chosen value : " ++ show value ++ "\nAction list : " ++ show chosenActionList) $ head chosenActionList
    where Result value chosenActionList = minimaxFromTreeMax [] evalTree

{-
    *** Part II (10pt) ***

    Extension of Part I.e, using alpha-beta pruning. You will need to change the 'minimax' function
    below to make it use this function.
    
-}

-- Same as above but now use alpha-beta pruning.
-- [Hint 1: Extend the helper function in I.e to keep track of alpha and beta.]
-- [Hint 2: Use the 'Result' datatype.]

minimaxABFromTreeMin :: [Action] -> Result -> Result -> EvalTree -> Result
minimaxABFromTreeMin actionList beta alpha (StateTree value []) = (Result (value) actionList)
minimaxABFromTreeMin actionList beta alpha (StateTree value childList@(c:cs)) 
    | (beta < alpha) = beta -- The reason that it is more and not more and equal is because there might be two or more moves that have same utility.
    | length childList == 1 =  min beta ( minimaxABFromTreeMax (actionList ++ [action]) (Result (-10000) []) beta child )
    | otherwise = minimaxABFromTreeMin actionList (min beta (minimaxABFromTreeMax (actionList ++ [action]) (Result (-10000) []) beta child)) alpha (StateTree value (cs))
    where (action,child) = c

 

minimaxABFromTreeMax :: [Action] -> Result -> Result -> EvalTree -> Result
minimaxABFromTreeMax actionList alpha beta (StateTree value []) = (Result value actionList)
minimaxABFromTreeMax actionList alpha beta (StateTree value childList@(c:cs)) 
    | (alpha > beta) = alpha -- The reason that it is more and not more and equal is because there might be two or more moves that have same utility.
    | length childList == 1 = max alpha ( minimaxABFromTreeMin (actionList ++ [action]) (Result (10000) []) alpha child )
    | otherwise =  minimaxABFromTreeMax actionList (max alpha (minimaxABFromTreeMin (actionList ++ [action]) (Result (10000) []) alpha child)) beta (StateTree value (cs))
    where (action,child) = c


minimaxABFromTree :: EvalTree -> Action
minimaxABFromTree evalTree = trace ("Chosen value : " ++ show value ++ "\nAction list : " ++ show chosenActionList) $ head chosenActionList
--     where Result value chosenActionList = minimax_ab (Result (-10000) []) (Result (10000) []) [] evalTree   
   where Result value chosenActionList = minimaxABFromTreeMax [] (Result (-10000) []) (Result (10000) []) evalTree


{-
    Putting everything together.
-}

-- Given depth for pruning (should be even).
depth :: Int 
depth = 2

-- Given breadth for pruning.
breadth :: Int 
breadth = 20

-- Function that combines all the different parts implemented in Part I.
minimax :: Game -> Action
minimax =
      minimaxABFromTree -- or 'minimaxABFromTree'
    . pruneBreadth breadth
    . highFirst
    . evalTree
    . pruneDepth depth
    . generateGameTree 

-- Given a game state, calls minimax and returns an action.
minimaxAction :: Board -> [Player] -> String -> Int -> Maybe Action
minimaxAction b ps _ r = let g = Game b ps in minimaxAction' g (minimax g)
    where 
        -- Goes through the list of actions until it finds a valid one. 
        minimaxAction' :: Game -> Action -> Maybe Action
        minimaxAction' g' (Move s)
            | validStepAction g' s = Just (Move s)
            | otherwise = error "Minimax chose an invalid action."
        minimaxAction' g' (Place w)
            | validWallAction g' w = Just (Place w)
            | otherwise = error "Minimax chose an invalid action."

-- Make minimaxPlayer in the usual way using 'minimaxAction'.
makeMinimaxPlayer :: String -> Cell -> Int -> [Cell] -> Player
makeMinimaxPlayer n c rws wps = Player {
    name = n,
    turn = 1,
    currentCell = c, 
    remainingWalls = rws,
    winningPositions = wps,
    isHuman = False,
    chooseAction = minimaxAction }
