

module CSP(
    CSP(..),
    Problem(..)
    ) where


import Data.Set (Set)


class Problem a where
    consistent  :: a -> Bool
    complete    :: a -> Bool

class Problem a => CSP a v d | a -> v, a -> d where
    legalValues         :: v -> a -> Set d
    relatedVariables    :: v -> a -> [v]
    addAssignment       :: v -> d -> a -> a

