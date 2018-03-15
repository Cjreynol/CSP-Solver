{-|
Module      : Solver
Description : Functions for solving board
Copyright   : (c) Chad Reynolds, 2018
-}
module Solver(
    recBacktracking
    ) where


import qualified    Data.List as List       (sortBy)
import              Data.Ord                (comparing)
import qualified    Data.Set as Set         (Set, size, toList)

import              CSP                     (CSP(..), Problem(..))


-- | Recursively attempts placements of digits in positions, backtracking 
-- when an invalid placement is made.  Uses MRV to direct position choices 
-- and LCV to direct digit choices to minimize the amount of backtracking.
recBacktracking :: CSP a v d => a -> a
recBacktracking board = recBacktracking' nextPos posValues board
    where
        nextPos = minRemainingValues board
        posValues = leastConstrainingValue nextPos board

recBacktracking' :: CSP a v d => v -> [d] -> a -> a
recBacktracking' pos [] board = board
recBacktracking' pos (x:xs) board 
    | complete board = board
    | consistent board = 
        case complete recResult of 
            True -> recResult
            False ->  nextTry
    | otherwise = nextTry
    where 
        updatedBoard = addAssignment pos x board
        nextPos = minRemainingValues updatedBoard
        posValues = leastConstrainingValue nextPos updatedBoard
        recResult = recBacktracking' nextPos posValues updatedBoard
        nextTry = recBacktracking' pos xs board

minRemainingValues :: CSP a v d => a -> v
minRemainingValues board = fst . head . reverse  $ List.sortBy (comparing snd) (valueCount board)

leastConstrainingValue :: CSP a v d => v -> a -> [d]
leastConstrainingValue pos board = map fst $ List.sortBy (comparing snd) valsCounts
    where 
        vals = Set.toList $ legalValues pos board
        relPositions = relatedVariables pos board
        valsBoards = Prelude.zip vals $ map (\x -> addAssignment pos x board) vals
        valsCounts = Prelude.zip vals $ map ((\b -> sum (map (\p -> Set.size (legalValues p b)) relPositions)) . snd) valsBoards

