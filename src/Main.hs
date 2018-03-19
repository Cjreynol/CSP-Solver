{-|
Module      : Main
Description : Command-line interface for solver
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module Main(
    main
    ) where


import Data.Char            (toLower)
import Data.List            (uncons)
import System.Environment   (getArgs)

import Solver               (recBacktracking)
import Sudoku.SudokuBoard   (SudokuBoard)


-- | Main loop of execution.
-- 
-- Results depend on the number of command-line arguments available after the 
-- problem type arg:
-- 
-- One arg:     Convert string to problem, solve, output.
--
-- Two args:    Convert both to problem, solve first, compare, output 
--              comparison.
--
-- Zero args:   Prompt for input string, execute one arg logic.
main :: IO ()
main = do
        args <- getArgs
        let (typeStr,argList) = maybe ("", []) id (uncons args) in
            processInput (fmap toLower typeStr) argList

processInput :: String -> [String] -> IO ()
processInput typeStr args = case length args of 
                                0 -> loopSolving typeStr
                                1 -> putStrLn $ oneArg typeStr (head args)
                                2 -> putStrLn $ twoArg typeStr (head args) (head . tail $ args)
                                _ -> error "Unexpected number of inputs, 1 or 2 expected"

loopSolving :: String -> IO ()
loopSolving typeStr = do
                        input <- getLine
                        case input == "" of
                            True -> loopSolving typeStr
                            False -> processInput typeStr [input]
                        loopSolving typeStr

oneArg :: String -> String -> String
oneArg typeStr s 
    | typeStr == "sudoku" = show . recBacktracking $ (read s :: SudokuBoard)
    | otherwise = error "Unexpected problem type."

twoArg :: String -> String -> String -> String
twoArg typeStr s1 s2 
    | typeStr == "sudoku" = let prob1 = recBacktracking $ (read s1 :: SudokuBoard)
                                prob2 = recBacktracking $ (read s2 :: SudokuBoard) in 
                                case prob1 == prob2 of
                                    True -> "Matching Solutions"
                                    False -> "Different Solutions"
    | otherwise = error "Unexpected problem type."

