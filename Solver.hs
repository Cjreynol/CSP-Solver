module Solver(
    recBacktracking
    ) where


import Sudoku

recBacktracking :: SudokuBoard -> SudokuBoard
recBacktracking board = recBacktracking' nextPos posValues board
    where
        nextPos = minRemainingValues board
        posValues = leastConstrainingValue nextPos board

recBacktracking' :: BoardPosition -> [SudokuDigit] -> SudokuBoard -> SudokuBoard
recBacktracking' pos [] board = board
recBacktracking' pos (x:xs) board 
    | solvedBoard board = board
    | validBoard board = 
        case solvedBoard recResult of 
            True -> recResult
            False ->  nextTry
    | otherwise = nextTry
    where 
        updatedBoard = updateBoard pos x board
        nextPos = minRemainingValues updatedBoard
        posValues = leastConstrainingValue nextPos updatedBoard
        recResult = recBacktracking' nextPos posValues updatedBoard
        nextTry = recBacktracking' pos xs board

