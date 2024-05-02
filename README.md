# Minesweeper-Game

## Problem Statement
The objective of this project is to implement the classic Minesweeper game
in Haskell, complete with a user interface. In addition, develop two different
solvers that attempt to solve the game. The first solver should make moves at
random, while the second solver should use a more sophisticated strategy.

### Requirements
- Implement the Minesweeper game adhering to traditional rules and mechanics. The game should be playable by a human user.
- The game should include a grid of cells, some of which contain mines. The player should be able to uncover cells, with the goal of clearing the board without detonating any mines.
- The game should display a number in a cell indicating how many adjacent cells contain mines. If none of the adjacent cells contain mines, the cell should be blank, and all adjacent cells should be automatically uncovered.
- The player should be able to place flags on cells they suspect contain mines.
- There should be an option to select any difficulty from: Easy, Medium, Hard and Custom. In custom level, the user is allowed to select the grid size and mine density in the grid.
- Develop a user interface for the game. The UI should display the game board and allow the user to make moves (uncover a cell, place a flag).
- The UI should update to reflect the game state after each move.
- The UI should display a message when the game is over (either the player has won by uncovering all non-mine cells, or the player has lost by uncovering a mine).
- Implement a “random” solver. This solver should select cells at random to uncover or place flags.
- Implement a “smart” solver. This solver should use a strategy to determine which cells to uncover or place flags. The strategy could be based on the numbers revealed in uncovered cells (which indicate how many adjacent cells contain mines), the positions of flagged cells, and/or other factors.

### How to execute ?
To compile, run the following command: <br>
` ghc -o minesweeper .\minesweeper.hs -package text -package aeson -package random -package scotty ` <br>
This will generate a minesweeper.exe file. <br>
The run the following command: ` ./minesweeper.exe ` <br>
Then open ` minesweeper.html ` and start playing :)
