module SudokuDigit(
    SudokuDigit(Blank),
    charToDigit,
    sudokuDomain
    ) where


import Data.Set (Set, fromList)


data SudokuDigit =  Blank | One | Two | 
                    Three | Four | Five | 
                    Six | Seven | Eight | Nine
                    deriving (Eq, Ord)

instance Show SudokuDigit where
    show = \x -> [digitToChar x]

digitToChar :: SudokuDigit -> Char
digitToChar (Blank) = ' '
digitToChar (One) = '1'
digitToChar (Two) = '2'
digitToChar (Three) = '3'
digitToChar (Four) = '4'
digitToChar (Five) = '5'
digitToChar (Six) = '6'
digitToChar (Seven) = '7'
digitToChar (Eight) = '8'
digitToChar (Nine) = '9'

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

sudokuDomain :: Set SudokuDigit
sudokuDomain = fromList [One, Two, Three, Four, Five, Six, Seven, Eight, Nine]

