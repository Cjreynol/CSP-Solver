module Solver(
    recBacktracking
    ) where


import Sudoku

recBacktracking :: SudokuBoard -> SudokuBoard
recBacktracking board
    | solvedBoard board = board
    | otherwise = undefined




{-
until the board is solved:
    select a variable to update
    progress through values for that variable 
    if the assignment is not valid don't recurse,
        if it is then do
-}
