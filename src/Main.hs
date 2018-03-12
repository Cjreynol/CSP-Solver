import System.Environment   (getArgs)
import Solver               (recBacktracking)
import SudokuBoard          (strToBoard)


main :: IO ()
main = do
        args <- getArgs
        putStrLn $ processInput args

processInput :: [String] -> String
processInput args = case length args of 
                    1 -> oneArg (head args)
                    2 -> twoArg (head args) (head . tail $ args)
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
                                



{-
Need an alternate path for when there are no args to prompt for input
-}

