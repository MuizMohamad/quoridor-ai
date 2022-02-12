# Quoridor in Haskell 

Quoridor game, in addition to AI to play with user.  
Applying search techniques and some position evaluation on the game tree in order to choose the best move.

### Installation instructions 

You need GHC, the Cabal build system and the Stack tool. See [https://www.haskell.org/platform/](https://www.haskell.org/platform/). 

### Playing the game 

The easiest way to play the game is to go to the `src` directory, run `ghci Main` and execute the `main` function.

Actions in the game:  
• Moving a step is recorded as the destination cell. For instance, the starting player's first  
action could be `d1`, which would mean they've moved to the left.  

1 https://en.wikipedia.org/wiki/Quoridor  
2 https://quoridorstrats.wordpress.com/notation/  
  
• Placing a wall is recorded as a cell plus `h` (horizontal) or `v` (vertical). The wall is placed
in between the cell and its top-right neighbour, in the specified direction.  

Examples:
 `e3v` corresponds to the wall between columns `e` and `f` spanning columns `3` and `4`.

### Run the tests 

There are two test suites:
* Basic tests (`stack test :basic-tests`).
* Minimax tests (`stack test :minimax-tests`).
