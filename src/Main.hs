import System.Environment   (getArgs)
import Solver               (recBacktracking)
import SudokuBoard          (strToBoard)


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
    where
        oneArg :: String -> String
        oneArg s = show . recBacktracking . strToBoard $ s
        twoArg :: String -> String -> String
        twoArg s1 s2 = let  boardIn = recBacktracking . strToBoard $ s1
                            boardSol = recBacktracking . strToBoard $ s2 in
                                case boardIn == boardSol of
                                    True -> "Correct Solution"
                                    False -> "Incorrect Solution"
                                
loopSolving :: IO ()
loopSolving = do
                args <- getLine
                case args == "" of
                    True -> loopSolving
                    False -> processInput [args]
                loopSolving

