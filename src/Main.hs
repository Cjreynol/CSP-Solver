{-|
Module      : Main
Description : Command-line interface for solver
Copyright   : (c) Chad Reynolds, 2018
-}
module Main(
    main
    ) where


import System.Environment   (getArgs)

import Solver               (recBacktracking)
import SudokuBoard          (strToBoard)


-- | Main loop of execution.
-- Depending on the number of command-line arguments are available:
-- 
-- One arg:     Convert string to board, solve, output.
--
-- Two args:    Convert both to boards, solve first, compare, output 
--              comparison.
--
-- Zero args:   Prompt for input string, execute one arg logic.
main :: IO ()
main = do
        args <- getArgs
        processInput args

processInput :: [String] -> IO ()
processInput args = case length args of 
                    0 -> loopSolving
                    1 -> putStrLn $ oneArg (head args)
                    2 -> putStrLn $ twoArg (head args) (head . tail $ args)
                    _ -> error "Unexpected number of inputs, 1 or 2 expected"

loopSolving :: IO ()
loopSolving = do
                input <- getLine
                case input == "" of
                    True -> loopSolving
                    False -> processInput [input]
                loopSolving

oneArg :: String -> String
oneArg s = show . recBacktracking . strToBoard $ s

twoArg :: String -> String -> String
twoArg s1 s2 = let  boardIn = recBacktracking . strToBoard $ s1
                    boardSol = recBacktracking . strToBoard $ s2 in
                        case boardIn == boardSol of
                            True -> "Correct Solution"
                            False -> "Incorrect Solution"
                                
