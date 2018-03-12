module SudokuBoard(
    SudokuBoard(Board),
    BoardPosition,
    SudokuDigit,
    getAllRelatedDigits,
    getAllRelatedPositions,
    getDigit,
    initializeBoard,
    strToBoard,
    updateBoard,
    validBoard,
    solvedBoard
    ) where


import              Data.Sequence as Seq    (Seq(..), adjust', fromList, 
                                                index, replicate, 
                                                update, (><))
import qualified    Data.Set as Set         (Set, empty, insert, member)

import              SudokuDigit             (SudokuDigit(Blank), charToDigit)


data SudokuBoard = Board (Seq (Seq SudokuDigit))
    deriving (Eq)

instance Show SudokuBoard where
    show = boardShow

boardShow :: SudokuBoard -> String
boardShow (Board Empty) = ""
boardShow (Board (x :<| Empty)) = rowShow x
boardShow (Board (x :<| xs)) = rowShow x ++ "\n" ++ boardShow (Board xs)

rowShow :: Seq SudokuDigit -> String
rowShow (Empty) = ""
rowShow (x :<| Empty) = show x
rowShow (x :<| xs) = show x ++ " | " ++ rowShow xs

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

strToBoard :: String -> SudokuBoard
strToBoard s = initializeBoard . strToInitList $ s

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

getAllRelatedPositions :: BoardPosition -> [BoardPosition]
getAllRelatedPositions pos@(r,c) = (getRowPositions r) ++ (getColPositions c) ++ (getCagePositions (cagePosFromBoardPos pos))

checkIfSeqGen :: Bool -> Seq SudokuDigit -> Bool
checkIfSeqGen solveCheck digits = helper digits Set.empty
    where
        helper :: Seq SudokuDigit -> Set.Set SudokuDigit -> Bool
        helper (Empty) set = True
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

