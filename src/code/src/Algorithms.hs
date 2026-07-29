module Algorithms 
  ( mergesort
  ) where

-- |Mergesort a list.
mergesort :: Ord a => [a] -> [a]
#ifdef TEMPLATE
mergesort = undefined
#else
mergesort xs = mergeAll (runs xs)
  where
    -- Finds all consecutive non-decreasing subsequences in the input, e.g.,
    -- runs [1, 3, 4, 2, 5] = [[1, 3, 4], [2, 5]]
    runs []     = [[]]
    runs [x]    = [[x]]
    runs (x:xs)
      | x < head r = (x:r) : rs
      | otherwise  = [x] : (r:rs)
      where (r:rs) = runs xs

    -- Merges two sorted lists
    merge2 xs     []                 = xs
    merge2 []     ys                 = ys
    merge2 (x:xs) (y:ys) | x < y     = x : merge2 xs (y:ys)
                         | otherwise = y : merge2 (x:xs) ys

    -- Takes a list of runs and merges two consecutive sorted lists until the
    -- the end of the list of runs is reached.
    --
    -- Why is this more efficient than always merging the first two lists?
    reduce []       = []
    reduce [x]      = [x]
    reduce (x:y:xs) = merge2 x y : reduce xs

    -- Apply reduce until a fixed point is reached.
    mergeAll [x]      = x
    mergeAll xs = mergeAll (reduce xs)
#endif

