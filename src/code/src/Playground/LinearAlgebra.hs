module Playground.LinearAlgebra where

import Data.Bifunctor (bimap)
import Data.List (singleton, transpose)
import Data.Maybe (fromMaybe)



newtype Mat a = Mat [[a]]
  deriving (Eq, Show)

instance Functor Mat where
  -- Applies f to each component of A
  fmap f (Mat a) = Mat (fmap (fmap f) a)

-- Computes all columns of A
cols :: Mat a -> [Mat a]
cols (Mat as) = map (Mat . transpose . singleton) (transpose as)

-- Computes size of A
size :: Mat a -> (Int, Int)
size (Mat as) = (length as, length (head as))

-- Transposes A
trans :: Mat a -> Mat a
trans (Mat as) = Mat (transpose as)

-- Computes identity of size n x n
identity :: Num a => Int -> Mat a 
identity 1 = Mat [[1]]
identity n = Mat ((1 : replicate (n - 1) 0) : map (0 :) i)
  where Mat i = identity (n - 1)


-- |Vertically stacks two matrices.
vstack :: Mat a -> Mat a -> Mat a
#ifdef TEMPLATE
vstack = error "not implemented"
#else
vstack (Mat as) (Mat bs) = Mat (as ++ bs)
#endif

-- |Horizontally stacks two matrices A, B to |A B|
hstack :: Mat a -> Mat a -> Mat a
#ifdef TEMPLATE
hstack = error "not implemented"
#else
hstack (Mat [])       (Mat [])       = Mat []
hstack (Mat (a : as)) (Mat (b : bs)) = vstack (Mat [a ++ b]) (hstack (Mat as) (Mat bs))

-- or later: hstack (Mat as) (Mat bs) = Mat (zipWith (++) as bs)
#endif

-- |Computes
-- ```
-- A = |a B|
-- ```
-- where a is the first column of A and B the remaining submatrix.
hsplit1 :: Mat a -> (Mat a, Mat a)
#if TEMPLATE
hsplit1 = undefined
#else
hsplit1 (Mat [])                = (Mat [], Mat [])
hsplit1 (Mat ((a11 : a1x) : a)) = (Mat ([a11] : ax1), Mat (a1x : axx))
  where (Mat ax1, Mat axx) = hsplit1 (Mat a)
#endif

-- |Computes
-- ```
-- A = |B a|
-- ```
-- where a is the last column of A and B the remaining submatrix.
hsplitn :: Mat a -> (Mat a, Mat a)
#if TEMPLATE
hsplitn = error "not implemented"
#else
hsplitn (Mat [])       = (Mat [], Mat [])
hsplitn (Mat (a1 : a)) = (Mat (ax1 : axx), Mat ([an1] : axn))
  where
    ax1 = init a1
    an1 = last a1
    (Mat axx, Mat axn) = hsplitn (Mat a)
#endif
 
-- |Computes
-- ```
-- A = |B|
--     |a|
-- ```
-- where a is the last row of A and B the remaining submatrix.
vsplitn :: Mat a -> (Mat a, Mat a)
#ifdef TEMPLATE
vsplitn = error "not implemented"
#else
vsplitn (Mat [])        = (Mat [], Mat [])
vsplitn (Mat [a])       = (Mat [], Mat [a])
vsplitn (Mat (a1x : a)) = (Mat (a1x : axx), manx)
  where (Mat axx, manx) = vsplitn (Mat a)
#endif

-- |Computes 
-- ```
-- A = |a11 a1x|
--     |ax1 axx| 
-- ```
unblock :: Mat a -> (Mat a, Mat a, Mat a, Mat a)
#if TEMPLATE
unblock = error "not implemented"
#else
unblock (Mat ((a11 : a1x) : axx)) = (Mat [[a11]], Mat [a1x], max1, maxx)
  where (max1, maxx) = hsplit1 (Mat axx)
#endif

-- |Computes 
-- ```
-- A = |axx anx|
--     |axn ann| 
-- ```
unblock' :: Mat a -> (Mat a, Mat a, Mat a, Mat a)
#ifdef TEMPLATE
unblock' = error "not implemented"
#else
unblock' a = (maxx, manx, maxn, mann)
  where 
    (ma, man) = hsplitn a
    (maxx, manx) = vsplitn ma
    (maxn, mann) = vsplitn man
#endif
    
-- |Computes 
-- ```
-- |A B| 
-- |C D|
-- ```
-- given A, B, C, D.
block :: (Mat a, Mat a, Mat a, Mat a) -> Mat a
#ifdef TEMPLATE
block = error "not implemented"
#else
block (a, b, c, d) = vstack (hstack a b) (hstack c d)
#endif


-- Computes dot product of two lists 
dot :: Num a => [a] -> [a] -> a
dot a b = foldl1 (+) (zipWith (*) a b)

instance Num a => Num (Mat a) where
  Mat a + Mat b = Mat (zipWith (zipWith (+)) a b)
  Mat a - Mat b = Mat (zipWith (zipWith (-)) a b)
  Mat a * Mat b = Mat [map (dot r) (transpose b) | r <- a]

  abs = undefined
  signum = undefined
  fromInteger = undefined




-- Computes LU decomposition of A
lu :: (Eq a, Fractional a) => Mat a -> Maybe (Mat a, Mat a)
lu a@(Mat [[a11]]) 
  | a11 == 0  = Nothing 
  | otherwise = Just (Mat [[1]], a)
lu a@(Mat as)
  | a11 == 0  = Nothing
  | otherwise = fmap (bimap (block . (l11, l1x, lx1,)) (block . (r11, r1x, rx1,)))
                     (lu (axx - lx1 * r1x))
  where
    (Mat [[a11]], a1x, ax1, axx) = unblock a

    r11 = Mat [[a11]]
    r1x = a1x
    rx1 = fmap (const 0) ax1

    l11 = Mat [[1]]
    lx1 = fmap (/ a11) ax1
    l1x = fmap (const 0) a1x


-- Computes main diagonal of A
diag :: Mat a -> [a]
diag (Mat []) = []
diag (Mat ([]:_)) = error "ill-shaped matrix"
diag (Mat ((d:_):rs)) = d : diag (Mat (map tail rs))


-- Computes det(A)
det :: (Eq a, Fractional a) => Mat a -> a
det = fromMaybe 0 . fmap (product . diag) . fmap snd . lu


-- Solves Ux = b
backward :: Fractional a => Mat a -> Mat a -> Mat a
backward _ (Mat []) = Mat []
backward u b        = Mat (xx ++ [[xn]])
  where
    (uxx, uxn, _, Mat [[unn]]) = unblock' u
    (_, bx, _, Mat [[bn]]) = unblock' b
    xn = bn / unn
    Mat xx = backward uxx (bx - fmap (xn *) uxn)


-- Solves Lx = b
forward :: Fractional a => Mat a -> Mat a -> Mat a
forward _ (Mat []) = Mat []
forward l b        = Mat ([x1] : xx)
  where
    (Mat [[l11]], _, lx1, lxx) = unblock l
    (Mat [[b11]], _, bx, _) = unblock b
    x1 = b11 / l11
    Mat xx = forward lxx (bx - fmap (x1 *) lx1)


-- Solves AX = B
solve :: (Eq a, Fractional a) => Mat a -> Mat a -> Maybe (Mat a)
solve a b = fmap solve' (lu a)
  where solve' (l, u) = foldr1 hstack (map (backward u . forward l) (cols b))


-- Computes A^(-1)
inv :: (Eq a, Fractional a) => Mat a -> Maybe (Mat a)
inv a = solve a (identity n)
  where (n, _) = size a

