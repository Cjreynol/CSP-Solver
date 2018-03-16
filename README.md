# Constraint Satisfaction Problem Solver  
  
A Haskell implementation of a backtracking CSP solver that relies on MRV and 
LCV to improve assignment choices and limit backtracking.  The solver expects 
CSP instances, which provides the necessary functions needed for the 
backtracking algorithm.  A Sudoku puzzle instance is included, and can be 
tested from the command-line.
  
## To Build 
This project was written and tested using GHC version 8.2.2, but should work 
with a version of GHC that supports:  
  
- pattern synonyms and the constructors for Sequence(Empty, :<|, |>:)
- The language extensions MultiParamTypeClass, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances
  
To build, running the `make` command in the project directory to compile the 
solver executable.  Running `make all` will compile the haddock docs as well.  
  
## To Run  
Run the executable from the command line, with arguments as follows:  
`./solver problem_type [problem_to_solve] [problem_to_compare]`  
  
- Problem type - Currently only supports `sudoku` argument  
- Running with only the problem\_to\_solve argument will output the solved 
problem.
- Running with both args will solve both problems and output a comparison.  This 
can be used to verify a solution, comparing it to the output of the solver.  
- No optional arguments starts a loop that has single arg behavior.  
  
Sudoku puzzles are expected to be a string of 81 characters containing 
numbers 1-9 and another character representing blanks.  Puzzles in the proper 
format can easily be generated on this useful site 
(https://qqwing.com/generate.html).  
