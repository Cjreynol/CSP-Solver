{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}


{-|
Module      : SudokuBoard
Description : Datatype and functions for representing/manipulating a sudoku 
                board
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module Sudoku.SudokuBoard(
    SudokuBoard,
    ) where


import              Data.Sequence as Seq    (Seq(..), adjust', fromList, 
                                                index, replicate, 
                                                update, (><), (<|))
import qualified    Data.Set as Set         (Set, delete, empty, insert, 
                                                member, size)

import              CSP                     (CSP(..), Assignment(..))
import              Sudoku.SudokuDigit      (SudokuDigit(Blank), charToDigit,
                                                sudokuDomain)


-- | Contains the state of the board.
data SudokuBoard = Board (Seq (Seq SudokuDigit))
    deriving (Eq)

-- | Represents the row, column pairs used as index positions in the board.
type BoardPosition = (Int, Int)

boardSize :: Int
boardSize = 9

instance Show SudokuBoard where
    show = showBoard

instance Read SudokuBoard where
    readsPrec _ value = [((readBoard value), "")]

instance Assignment SudokuBoard where
    consistent = validBoard
    complete = solvedBoard

instance CSP SudokuBoard BoardPosition SudokuDigit where
    legalValues = legalDigits
    relatedVariables = \bp _ -> getAllRelatedPositions bp
    addAssignment = updateBoard
    minValueCount = minDigitCount

showBoard :: SudokuBoard -> String
showBoard (Board Empty) = ""
showBoard (Board (x :<| Empty)) = rowShow x
showBoard (Board (x :<| xs)) = rowShow x ++ "\n" ++ showBoard (Board xs)

rowShow :: Seq SudokuDigit -> String
rowShow (Empty) = ""
rowShow (x :<| Empty) = show x
rowShow (x :<| xs) = show x ++ " | " ++ rowShow xs

emptyBoard :: SudokuBoard
emptyBoard = Board (Seq.replicate boardSize $ Seq.replicate boardSize Blank)

initializeBoard :: [(BoardPosition, SudokuDigit)] -> SudokuBoard
initializeBoard updates = foldr helper emptyBoard updates
    where 
        helper :: (BoardPosition, SudokuDigit) -> SudokuBoard -> SudokuBoard
        helper (pos,digit) b = updateBoard pos digit b

strToInitList :: String -> [(BoardPosition, SudokuDigit)]
strToInitList s = zipWith helper [0..] s
    where
        helper :: Int -> Char -> (BoardPosition, SudokuDigit)
        helper n ch = (indexToBoardPos n, charToDigit ch)
        indexToBoardPos :: Int -> BoardPosition
        indexToBoardPos n = (div n boardSize, mod n boardSize)

-- | Expects a string of 81 characters of digits 1-9.  Any other char is 
-- interpreted as a Blank.
readBoard :: String -> SudokuBoard
readBoard s = initializeBoard . strToInitList $ s

updateBoard :: BoardPosition -> SudokuDigit -> SudokuBoard -> SudokuBoard
updateBoard (r,c) digit (Board b) = Board $ adjust' (update c digit) r b

getDigit :: BoardPosition -> SudokuBoard -> SudokuDigit
getDigit (r,c) (Board b) = index (index b r) c

getRow :: Int -> SudokuBoard -> Seq SudokuDigit
getRow n (Board b) = index b n

getCol :: Int -> SudokuBoard -> Seq SudokuDigit
getCol n (Board b) = fmap (\x -> index x n) b

getCage :: Int -> SudokuBoard -> Seq SudokuDigit
getCage n b = fromList $ fmap (\z -> getDigit z b) $ getCageIndices (r,c)
    where
        r = (div n 3) * 3
        c = (mod n 3) * 3

getCageIndices :: BoardPosition -> [BoardPosition]
getCageIndices (r,c) = [(x,y) | x <- [startR..endR], y <- [startC..endC]]
    where
        startR = (div r 3)  * 3
        endR = startR + 2
        startC = (div c 3) * 3
        endC = startC + 2

-- | Returns a sequence of all the digits in the same row/col/cage as the 
-- given position, including the given position.
getAllRelatedDigits :: BoardPosition -> SudokuBoard -> Seq SudokuDigit
getAllRelatedDigits (r,c) b = row >< col >< cage
    where
        row = getRow r b
        col = getCol c b
        cage = getCage cageIndex b
        cageIndex = ((div r 3) * 3) + (mod (div c 3) 3)

-- | Returns a list of all the board positions in the same row/col/cage as the 
-- given position, including the given position.
getAllRelatedPositions :: BoardPosition -> [BoardPosition]
getAllRelatedPositions pos@(r,c) = row ++ col ++ cage
    where
        row = [(x,y) | x <- [r], y <- [0..8]]
        col = [(x,y) | x <- [0..8], y <- [c]]
        cage = getCageIndices pos

checkGen :: Bool -> (Int -> SudokuBoard -> Seq SudokuDigit) -> SudokuBoard -> Bool
checkGen solveCheck f b = foldr (&&) True uniqueSeqs
    where
        relSeqs = fmap (\x -> f x b) [0..8]
        uniqueSeqs = fmap (\x -> allUnique x Set.empty) relSeqs
        -- | Tests for uniqueness of elements in the sequence, but relies on 
        -- some Sudoku specific knowledge to improve the runtime
        allUnique :: Seq SudokuDigit -> Set.Set SudokuDigit -> Bool
        allUnique (Empty) _ = True
        allUnique (x:<|xs) set 
            | x == Blank = (not solveCheck) && allUnique xs set
            | not (Set.member x set) = allUnique xs $ Set.insert x set
            | otherwise = False

validBoard :: SudokuBoard -> Bool
validBoard b = validRows && validCols && validCages
    where
        validRows = checkGen False getRow b
        validCols = checkGen False getCol b
        validCages = checkGen False getCage b

solvedBoard :: SudokuBoard -> Bool
solvedBoard b = solvedRows && solvedCols && solvedCages
    where
        solvedRows = checkGen True getRow b
        solvedCols = checkGen True getCol b
        solvedCages = checkGen True getCage b

-- | Returns the legal values assignments for a given position on the board
legalDigits :: BoardPosition -> SudokuBoard -> Set.Set SudokuDigit
legalDigits pos b 
    | getDigit pos b == Blank = foldr helper sudokuDomain (getAllRelatedDigits pos b)
    | otherwise = Set.empty
    where 
        helper :: SudokuDigit -> Set.Set SudokuDigit -> Set.Set SudokuDigit
        helper digit set 
            | digit == Blank = set
            | otherwise = Set.delete digit set

-- | Is an implementation of the MRV algorithm specific to SudokuBoard.  
-- Keeps track of the board position with the minimum available assignments, 
-- with a default value of (0,0) if they are all equal.
minDigitCount :: SudokuBoard -> BoardPosition
minDigitCount (Board b) = helper (-1) (0,0) (0,0) b
    where 
        helper :: Int -> BoardPosition ->  BoardPosition -> Seq (Seq SudokuDigit) -> BoardPosition
        helper _ minPos _ (Empty) = minPos
        helper minCnt minPos (r,_) ((Empty) :<| xs) = helper minCnt minPos ((r+1),0) xs
        helper minCnt minPos pos@(r,c) ((x :<| xs) :<| xss) 
            | x == Blank = let  newVals = Set.size (legalValues pos (Board b))
                                nextPos = (r,(c+1))
                                nextSeq = (xs <| xss) in 
                                case (minCnt == (-1)) || (newVals < minCnt) of
                                    True -> helper newVals pos nextPos nextSeq
                                    False -> helper minCnt minPos nextPos nextSeq
            | otherwise = helper minCnt minPos (r,(c+1)) (xs <| xss)

