{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}


{-|
Module      : CSP
Description : Typeclass definitions for for Constraint Satisfaction Problems
Copyright   : (c) Chad Reynolds, 2018
-}
module CSP(
    CSP(..),
    Assignment(..)
    ) where


import Data.Set (Set)


-- | Represents the interface required by CSP assignments so that it can be 
-- checked that it is valid but unsolved or valid and solved, respectively
class Assignment a where
    consistent  :: a -> Bool
    complete    :: a -> Bool

-- | Represents the interfaces needed by the current Solver so that the
-- recursive backtracking implementation can work using MRV and LCV
class Assignment a => CSP a v d | a -> v, a -> d where
    legalValues         :: v -> a -> Set d
    relatedVariables    :: v -> a -> [v]
    addAssignment       :: v -> d -> a -> a
    minValueCount       :: a -> v

