# Sudoku Solver  
  
A Haskell implementation of a Sudoku puzzle solver.
  
## To Build  
This project was written and tested using GHC version 8.2.2, but should work 
with a version of GHC that supports pattern synonyms and a version of base 
that includes the pattern matching constructors for 
Sequences(Empty, :<|, |>:).  
  
To build, running the `make` command in the project directory should execute 
the code in the Makefile and compile the solver executable.  
  
## To Run  
Run the executable from the command line, with arguments as follows:  
`./solver [puzzle_to_solve] [solution_to_puzzle]`  
  
Running with zero arguments will loop a prompt for the single argument 
functionality.  One argument will output the solved board.  With two it will 
verify the solution with it's own result and outputting the comparison.  
  
Puzzles and solutions are expected to be a string of 81 characters containing 
numbers 1-9 and another character representing blanks.  Puzzles in the proper 
format can easily be generated on this useful site 
(https://qqwing.com/generate.html).  
