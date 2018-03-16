{-|
Module      : SudokuDigit
Description : Datatype for domain of SudokuBoard "squares"
Copyright   : (c) Chad Reynolds, 2018
-}
module Sudoku.SudokuDigit(
    SudokuDigit(Blank),
    charToDigit,
    sudokuDomain
    ) where


import Data.Set (Set, fromList)


-- | Covers the available assignments for a SudokuBoard.
--
-- Used instead of Ints to limit potential errors  when creating boards.
data SudokuDigit =  Blank | One | Two | 
                    Three | Four | Five | 
                    Six | Seven | Eight | Nine
                    deriving (Eq, Ord)

instance Show SudokuDigit where
    show = showDigit

instance Read SudokuDigit where
    readsPrec _ (x:xs) = [((charToDigit x),xs)]
    readsPrec _ _ = []

showDigit :: SudokuDigit -> String
showDigit (Blank) = " "
showDigit (One) = "1"
showDigit (Two) = "2"
showDigit (Three) = "3"
showDigit (Four) = "4"
showDigit (Five) = "5"
showDigit (Six) = "6"
showDigit (Seven) = "7"
showDigit (Eight) = "8"
showDigit (Nine) = "9"

-- | Converts Char to SudokuDigit for encoding Strings to SudokuBoards.
charToDigit :: Char -> SudokuDigit
charToDigit c = case c of
                    '1' -> One
                    '2' -> Two
                    '3' -> Three
                    '4' -> Four
                    '5' -> Five
                    '6' -> Six
                    '7' -> Seven
                    '8' -> Eight
                    '9' -> Nine
                    _ -> Blank

-- | The domain of values that are legal to place in a SudokuBoard.  
-- Excludes Blank, as this is a placeholder for unsolved positions.
sudokuDomain :: Set SudokuDigit
sudokuDomain = fromList [One, Two, Three, Four, Five, Six, Seven, Eight, Nine]

