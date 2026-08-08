module Playground.Tropical where

import Playground.LinearAlgebra (Mat(..))

-- Extended numbers with infinity
data WithInf a = Number a | Inf
  deriving (Eq, Ord)

instance Show a => Show (WithInf a) where
  show (Number a) = show a
  show Inf        = "∞"

instance (Num a, Ord a) => Num (WithInf a) where
#if TEMPLATE
#else
  Number a + Number b = Number (a + b)
  Number _ + Inf      = Inf
  Inf      + Number _ = Inf
  Inf      + Inf      = Inf

  Number a - Number b = Number (a - b)
  Number _ - Inf      = Inf
  Inf      - Number _ = Inf
  Inf      - Inf      = undefined

  Number a * Number b            = Number (a * b)
  Inf      * Number b | b > 0     = Inf
                      | otherwise = undefined
  Number a * Inf      | a > 0     = Inf
                      | otherwise = undefined
  Inf      * Inf                  = Inf

  negate (Number a) = Number (negate a)
  negate Inf        = undefined

  signum (Number a) = Number (signum a)
  signum Inf        = Number 1

  abs (Number a) = Number (abs a)
  abs Inf        = Inf

  fromInteger n = Number (fromInteger n)
#endif


-- | Wrapper type for operations on tropical numbers.
newtype Tropical a = Tropical { getTropical :: WithInf a }
  deriving (Eq, Ord)

instance Show a => Show (Tropical a) where
  show (Tropical x) = show x

instance (Num a, Ord a) => Num (Tropical a) where
#if TEMPLATE
#else
  Tropical a + Tropical b = Tropical (min a b)
  Tropical a - Tropical b = undefined
  Tropical a * Tropical b = Tropical (a + b)

  abs (Tropical a) = Tropical (abs a)
  signum (Tropical a) = Tropical (signum a)
  fromInteger n = Tropical (fromInteger n)
#endif


-- Computes the distance matrix of a weighted graph.
shortestPaths :: (Num a, Ord a) => Mat (WithInf a) -> Mat (WithInf a)
#if TEMPLATE
shortestPaths = error "not implemented"
#else
shortestPaths m@(Mat a) = fmap getTropical ((fmap Tropical m) ^ (length a))
#endif


-- | Weights for graph for testing.
--
-- The result of 'shortestPaths' should be
-- @shortestPaths w = Mat [[0,-2,1,3],[∞,0,3,5],[∞,∞,0,2],[∞,∞,∞,0]]@
w :: Mat (WithInf Int)
w = Mat [ [0, -2, Inf, 4]
        , [Inf, 0, 3, Inf]
        , [Inf, Inf, 0, 2]
        , [Inf, Inf, Inf, 0]
        ]

