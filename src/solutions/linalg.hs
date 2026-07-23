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


-- Horizontally stacks two matrices A, B to |A B|
hstack :: Mat a -> Mat a -> Mat a
hstack (Mat as) (Mat bs) = Mat (zipWith (++) as bs)


-- Transposes A
trans :: Mat a -> Mat a
trans (Mat as) = Mat (transpose as)


-- Computes identity of size n x n
identity :: Num a => Int -> Mat a 
identity 1 = Mat [[1]]
identity n = Mat ((1 : replicate (n - 1) 0) : map (0 :) i)
  where Mat i = identity (n - 1)


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


-- Computes A = |a11 a1x|
--              |ax1 axx| 
unblock :: Mat a -> (Mat a, Mat a, Mat a, Mat a)
unblock (Mat a) = (Mat a11, Mat a1x, Mat ax1, Mat axx)
  where
    a11 = [[head (head a)]]
    a1x = [tail (head a)]
    ax1 = map (singleton . head) (tail a)
    axx = map tail (tail a)


-- Computes A = |axx anx|
--              |axn ann| 
unblock' :: Mat a -> (Mat a, Mat a, Mat a, Mat a)
unblock' (Mat a) = (Mat axx, Mat anx, Mat axn, Mat ann)
  where
    axx = map init (init a)
    anx = map (singleton . last) (init a)
    axn = [init (last a)]
    ann = [[last (last a)]]

    
-- Computes |a11 a1x| = A
--          |ax1 axx| 
block :: (Mat a, Mat a, Mat a, Mat a) -> Mat a 
block (Mat [[a11]], Mat a1x, Mat ax1, Mat axx) = Mat (b1 ++ bx)
  where
    b1 = map (a11 :) a1x
    bx = zipWith (++) ax1 axx


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

