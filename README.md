# Quoridor in Haskell 

Quoridor game, in addition to AI to play with user.
Applying search techniques and some position evaluation on the game tree in order to choose the best move.

### Installation instructions 

You need GHC, the Cabal build system and the Stack tool. See [https://www.haskell.org/platform/](https://www.haskell.org/platform/). 

### Playing the game 

The easiest way to play the game is to go to the `src` directory, run `ghci Main` and execute the `main` function.

### Run the tests 

There are two test suites:
* Basic tests (`stack test :basic-tests`).
* Minimax tests (`stack test :minimax-tests`).
