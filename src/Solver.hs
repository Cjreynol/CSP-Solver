{-|
Module      : Solver
Description : Functions for solving assignments
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module Solver(
    recBacktracking
    ) where


import qualified    Data.List as List       (sortBy)
import              Data.Ord                (comparing)
import              Data.Set                (size, toList)

import              CSP                     (CSP(..), Assignment(..))


-- | Recursively attempts assignment of values in variables, backtracking 
-- when an invalid assignment is made.  Uses MRV to direct variable choices 
-- and LCV to direct value choices to minimize the amount of backtracking.
recBacktracking :: CSP a v d => a -> a
recBacktracking assignments = recBacktracking' nextVar posValues assignments
    where
        nextVar = minRemainingValues assignments
        posValues = leastConstrainingValues nextVar assignments

recBacktracking' :: CSP a v d => v -> [d] -> a -> a
recBacktracking' _ [] assignments = assignments
recBacktracking' var (x:xs) assignments 
    | complete assignments = assignments
    | consistent assignments = 
        case complete recResult of 
            True -> recResult
            False ->  nextTry
    | otherwise = nextTry
    where 
        updatedAssignments = addAssignment var x assignments
        nextVar = minRemainingValues updatedAssignments
        posValues = leastConstrainingValues nextVar updatedAssignments
        recResult = recBacktracking' nextVar posValues updatedAssignments
        nextTry = recBacktracking' var xs assignments

-- | Returns the variable with the fewest available legal values available 
-- for assignment.
minRemainingValues :: CSP a v d => a -> v
minRemainingValues assignments = minValueCount assignments

-- | Returns a list of the legal values remaining for a given variable, 
-- ordered by available values left in all variables constrained by the given.
leastConstrainingValues :: CSP a v d => v -> a -> [d]
leastConstrainingValues var assignments = fmap fst $ List.sortBy (comparing snd) valsCounts
    where 
        vals = toList $ legalValues var assignments
        relVars = relatedVariables var assignments
        valsAssignments = zip vals $ fmap (\x -> addAssignment var x assignments) vals
        valsCounts = zip vals $ fmap ((\b -> sum (fmap (\p -> size (legalValues p b)) relVars)) . snd) valsAssignments

