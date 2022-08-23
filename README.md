# Quoridor in Haskell 

Quoridor game, in addition to very basic AI to play with user.  
Applying search techniques and some position evaluation on the game tree in order to choose the best move.

See [Quoridor: How to play](https://www.ultraboardgames.com/quoridor/game-rules.php) for the game rules in general.

Currently the depth of the game tree are set to 2 with breadth of 20.  
As the game tree grows exponentially large with higher depth and breadth, depth and breadth are set to be low value to reduce computation time.

Supported algorithms/players:

1. **Minimax**
2. **Negamax** (a variation of Minimax)
3. **Dumb** (random/trivial moves)
4. **Reed** (a player that start with Reed opening)
   
### Playing instructions

1. Run Main.exe
2. Choose type of player for both player
3. Start playing!
   
### Environment setup instructions 

You need GHC, the Cabal build system and the Stack tool. See [https://www.haskell.org/platform/](https://www.haskell.org/platform/). 

### Playing the game with GHCi 

The easiest way to play the game with GHCi is to go to the `src` directory, run `ghci Main` and execute the `main` function.

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
