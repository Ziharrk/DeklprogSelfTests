module NumberTheory
  ( gcd 
  , pf
  ) where

import Prelude hiding (gcd)


-- |Greatest common divisors of two integral numbers.
--
-- Depending on when you tackle this challenge, we might not have covered
-- ad-hoc polymorphism in the lectures yet. However, you can treat 'gcd' as a
-- function @Int -> Int -> Int@.
gcd :: Integral a => a -> a -> a 
#if TEMPLATE
gcd = undefined
#else
gcd x 0 = x
gcd x y = gcd x (x `mod` y)
#endif


-- |Prime factors of a given integral number.
pf :: Integral a => a -> [a]
#ifdef TEMPLATE
pf = undefined
#else
pf a = go a [2..a]
  where
    go 1 _      = []
    go a (p:xs) | a < p * p = [a]  -- optimization
                | r == 0    = p : go q (p:xs)
                | otherwise = go a xs
      where (q, r) = quotRem a p  -- same as (a `div` p, a `mod` p)


-- |Intersection of two sorted lists
intersect :: Ord a => [a] -> [a] -> [a]
intersect []     _      = []
intersect _      []     = []
intersect (x:xs) (y:ys)
  | x == y    = x : intersect xs ys
  | x < y     = intersect xs (y:ys)
  | otherwise = intersect (x:xs) ys

-- gcd :: Int -> Int -> Int
-- gcd a b = product (pf a `intersection` pf b)
#endif

