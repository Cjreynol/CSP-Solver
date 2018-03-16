{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}


{-|
Module      : SudokuBoard
Description : Datatype and functions for representing/manipulating a sudoku 
                board
Copyright   : (c) Chad Reynolds, 2018
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

-- | Represents the row, column pairs used as index positions in the board.
type BoardPosition = (Int, Int)

emptyBoard :: SudokuBoard
emptyBoard = Board (Seq.replicate 9 $ Seq.replicate 9 Blank)

initializeBoard :: [(BoardPosition, SudokuDigit)] -> SudokuBoard
initializeBoard updates = foldr helper emptyBoard updates
    where 
        helper :: (BoardPosition, SudokuDigit) -> SudokuBoard -> SudokuBoard
        helper (pos,digit) board = updateBoard pos digit board

strToInitList :: String -> [(BoardPosition, SudokuDigit)]
strToInitList s = zipWith helper [0..] s
    where
        helper :: Int -> Char -> (BoardPosition, SudokuDigit)
        helper n ch = (indexToBoardPos n, charToDigit ch)
        indexToBoardPos :: Int -> BoardPosition
        indexToBoardPos n = (div n 9, mod n 9)

-- | Converts a string into a sudoku board.
--
-- Expects a string of 81 characters of digits 1-9.  Any other char is 
-- interpreted as a Blank.
readBoard :: String -> SudokuBoard
readBoard s = initializeBoard . strToInitList $ s

updateBoard :: BoardPosition -> SudokuDigit -> SudokuBoard -> SudokuBoard
updateBoard (r,c) digit (Board board) = Board (adjust' (update c digit) r board)

getDigit :: BoardPosition -> SudokuBoard -> SudokuDigit
getDigit (r,c) (Board board) = index (index board r) c

getRow :: Int -> SudokuBoard -> Seq SudokuDigit
getRow n (Board board) = index board n

getCol :: Int -> SudokuBoard -> Seq SudokuDigit
getCol n (Board board) = fmap (\x -> index x n) board

cagePosFromBoardPos :: BoardPosition -> Int
cagePosFromBoardPos (r,c) = ((div r 3) * 3) + (mod (div c 3) 3)

getCage :: Int -> SudokuBoard -> Seq SudokuDigit
getCage n board = fromList $ map (\z -> getDigit z board) [(x,y) | x <- [startr..endr], y <- [startc..endc]]
    where
        startr = (div n 3) * 3
        startc = (mod n 3) * 3
        endr = startr + 2
        endc = startc + 2

-- | Returns a sequence of all the digits in the same row/col/cage as the 
-- given position, including the given position.
getAllRelatedDigits :: BoardPosition -> SudokuBoard -> Seq SudokuDigit
getAllRelatedDigits pos@(r,c) board = (getRow r board) >< (getCol c board) >< (getCage (cagePosFromBoardPos pos) board)

getRowPositions :: Int -> [BoardPosition]
getRowPositions n = [(r,c) | r <- [n], c <- [0..8]]

getColPositions :: Int -> [BoardPosition]
getColPositions n = [(r,c) | r <- [0..8], c <- [n]]

getCagePositions :: Int -> [BoardPosition]
getCagePositions n = [(x,y) | x <- [startr..endr], y <- [startc..endc]]
    where
        startr = (div n 3) * 3
        startc = (mod n 3) * 3
        endr = startr + 2
        endc = startc + 2

-- | Returns a list of all the board positions in the same row/col/cage as the 
-- given position, including the given position.
getAllRelatedPositions :: BoardPosition -> [BoardPosition]
getAllRelatedPositions pos@(r,c) = (getRowPositions r) ++ (getColPositions c) ++ (getCagePositions (cagePosFromBoardPos pos))

checkIfSeqGen :: Bool -> Seq SudokuDigit -> Bool
checkIfSeqGen solveCheck digits = helper digits Set.empty
    where
        helper :: Seq SudokuDigit -> Set.Set SudokuDigit -> Bool
        helper (Empty) _ = True
        helper (x:<|xs) set 
            | x == Blank = (not solveCheck) && helper xs set
            | not (Set.member x set) = helper xs (Set.insert x set)
            | otherwise = False

checkIfValidSeq :: Seq SudokuDigit -> Bool
checkIfValidSeq digits = checkIfSeqGen False digits

checkIfSolvedSeq :: Seq SudokuDigit -> Bool
checkIfSolvedSeq digits = checkIfSeqGen True digits

checkGen :: (Seq SudokuDigit -> Bool) -> (Int -> SudokuBoard -> Seq SudokuDigit) -> SudokuBoard -> Bool
checkGen f g board = foldr (\x y -> (f x) && y) True (map (\x -> g x board) [0..8])

validRows :: SudokuBoard -> Bool
validRows board = checkGen checkIfValidSeq getRow board

validCols :: SudokuBoard -> Bool
validCols board = checkGen checkIfValidSeq getCol board

validCages :: SudokuBoard -> Bool
validCages board = checkGen checkIfValidSeq getCage board

validBoard :: SudokuBoard -> Bool
validBoard board = (validRows board) && (validCols board) && (validCages board)

solvedRows :: SudokuBoard -> Bool
solvedRows board = checkGen checkIfSolvedSeq getRow board

solvedCols :: SudokuBoard -> Bool
solvedCols board = checkGen checkIfSolvedSeq getCol board

solvedCages :: SudokuBoard -> Bool
solvedCages board = checkGen checkIfSolvedSeq getCage board

solvedBoard :: SudokuBoard -> Bool
solvedBoard board = (solvedRows board) && (solvedCols board) && (solvedCages board)

-- | Returns the legal values assignments for a given position on the board
legalDigits :: BoardPosition -> SudokuBoard -> Set.Set SudokuDigit
legalDigits pos board 
    | getDigit pos board == Blank = foldr helper sudokuDomain (getAllRelatedDigits pos board)
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
minDigitCount (Board board) = helper (-1) (0,0) (0,0) board
    where 
        helper :: Int -> BoardPosition ->  BoardPosition -> Seq (Seq SudokuDigit) -> BoardPosition
        helper _ minPos _ (Empty) = minPos
        helper minCnt minPos (r,_) ((Empty) :<| xs) = helper minCnt minPos ((r+1),0) xs
        helper minCnt minPos pos@(r,c) ((x :<| xs) :<| xss) 
            | x == Blank = let  newVals = Set.size (legalValues pos (Board board))
                                nextPos = (r,(c+1))
                                nextSeq = (xs <| xss) in 
                                case (minCnt == (-1)) || (newVals < minCnt) of
                                    True -> helper newVals pos nextPos nextSeq
                                    False -> helper minCnt minPos nextPos nextSeq
            | otherwise = helper minCnt minPos (r,(c+1)) (xs <| xss)

