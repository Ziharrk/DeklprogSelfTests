{-# LANGUAGE PatternSynonyms #-}
module Polynomial 
  ( PolyE
  , Poly(..)
  , degree 
  , leading
  , absolute
  , polydiv
  , horner
  , divisors
  , roots
  ) where


import Prelude hiding (last, zipWith)
import Data.List (dropWhileEnd)
import Data.List.NonEmpty (last, toList, NonEmpty(..), singleton, zipWith)
#ifdef FFT
import Data.Complex
#endif


-- | Represents a polynomial.
data PolyE a = Const a
             -- ^ Constant polynomial
             | T
             -- ^ Variable
             | PolyE a :+: PolyE a
             -- ^ Sum of two polynomials
             | PolyE a :-: PolyE a
             -- ^ Subtraction of two polynomials
             | PolyE a :*: PolyE a
             -- ^ Product of two polynomials
  deriving Eq

-- Match precedences of (+), (-) and (*)
infixl 6 :+:
infixl 6 :-:
infixl 7 :*:

instance Show a => Show (PolyE a) where
  showsPrec _ (Const c) = shows c
  showsPrec _ T         = showChar 'T'
  showsPrec p (p1 :+: p2) = showParen (p > 6) (showsPrec 6 p1 . showString " + " . showsPrec 6 p2)
  showsPrec p (p1 :-: p2) = showParen (p > 6) (showsPrec 6 p1 . showString " - " . showsPrec 6 p2)
  showsPrec p (p1 :*: p2) = showParen (p > 7) (showsPrec 7 p1 . showString " * " . showsPrec 7 p2)

-- | This Num instance allows you to write polynomials in terms of (+), (-), (*)
-- and integers instead of (:+:), (:-:), (:*:) and Const c. E.g., you can
-- write `(T - 3) * (T + 2)` instead of `(T :-: Const 3) :*: (T :+: Const 2)`.
--
-- Sadly, we cannot map from PolyE to Poly directly.
instance Num a => Num (PolyE a) where
  (+) = (:+:)
  (-) = (:-:)
  (*) = (:*:)
  abs = undefined
  signum = undefined
  fromInteger = Const . fromInteger


-- | Adds two polynomials (without normalization) given their coefficients.
polyadd :: Num a => [a] -> [a] -> [a]
#ifdef TEMPLATE
polyadd = error "not implemented"
#else
polyadd []     ys     = ys
polyadd xs     []     = xs
polyadd (x:xs) (y:ys) = x + y : polyadd xs ys
#endif

-- | Subtracts two polynomials (without normalization) given their coefficients.
polysub :: Num a => [a] -> [a] -> [a]
#ifdef TEMPLATE
polysub = error "not implemented"
#else
polysub []     ys     = map negate ys
polysub xs     []     = xs
polysub (x:xs) (y:ys) = x - y : polysub xs ys
#endif


#ifdef FFT
fft :: [Complex Double] -> [Complex Double]
fft [x] = [x]
fft xs = zipWith (+) evenPart twiddles ++ zipWith (-) evenPart twiddles
  where
    n = length xs
    (evens, odds) = split xs
    evenPart = fft evens
    oddPart = fft odds
    twiddles = zipWith (*) oddPart [cis (-2 * pi * fromIntegral k / fromIntegral n) | k <- [0..n `div` 2 - 1]]

split :: [a] -> ([a], [a])
split []       = ([], [])
split [x]      = ([x], [])
split (x:y:xs) = let (xs1, xs2) = split xs
                  in (x : xs1, y : xs2)

ifft :: [Complex Double] -> [Complex Double]
ifft xs = (map (/ fromIntegral n) . map conjugate . fft . map conjugate) xs
  where n = length xs

nextPowerOfTwo :: Int -> Int
nextPowerOfTwo n = head (dropWhile (< n) (iterate (* 2) 1))

pad :: Integral a => Int -> [a] -> [Complex Double]
pad n xs = map ((:+ 0) . fromIntegral) xs ++ replicate (n - length xs) 0


#endif
-- | Multiplies two polynomials (without normalization) given their 
-- coefficients.
#ifdef FFT
polymul :: Integral a => [a] -> [a] -> [a]
#else
polymul :: Num a => [a] -> [a] -> [a]
#endif
#ifdef TEMPLATE
polymul = error "not implemented"
#else
#ifdef FFT
polymul p q = take (fromIntegral m) (map (round . realPart) (ifft (zipWith (*) fp fq)))
  where
    m = length p + length q - 1
    n = nextPowerOfTwo m
    fp = fft (pad n p)
    fq = fft (pad n q)
#else
polymul []     _  = []
polymul _      [] = []
polymul (a:as) bs = polyadd (map (a *) bs) (0 : polymul as bs)
#endif
#endif


-- | Represents a polynomial in normal form.
--
-- We call a polynomial `p :: PolyE a` in normal form, if it is of the form
-- `a_0 + a_1 * T + a_2 * T^2 + ... a_n T^n`. This type abandons `PolyE a`
-- representation and stores only the coefficients [a_0, a_1, a_2, ..., a_n].
--
-- We assume that the coefficients do not have trailing zeros.
newtype Poly a = MkPoly { coeffs :: NonEmpty a
                        -- ^ Retrieve the coefficients of a polynomial
                        }
  deriving (Eq, Show)

-- Smart constructor for polynomials
poly :: (Eq a, Num a) => [a] -> Poly a
poly []     = MkPoly (0 :| [])
poly (c:cs) = nf (MkPoly (c :| cs))

-- Normalizes a polynomial
nf :: (Eq a, Num a) => Poly a -> Poly a
nf (MkPoly (c :| cs)) = MkPoly (c :| dropWhileEnd (== 0) cs)

-- | Determines the degree of polynomial
degree :: Integral a => Poly a -> Int
degree p = length (coeffs p) - 1

-- | Retrieves the leading coefficient of a polynomial
leading :: Poly a -> a
leading = last . coeffs

-- | Retrieves the absolute term of a polynomial
absolute :: Poly a -> a
absolute (MkPoly (c :| _)) = c


-- | Normalizes a polynomial.
#ifdef FFT
fromPolyE :: Integral a => PolyE a -> Poly a
#else
fromPolyE :: (Eq a, Num a) => PolyE a -> Poly a
#endif
#ifdef TEMPLATE
fromPolyE = error "not implemented"
#else
fromPolyE p = poly (go p)
  where
    go (Const c)   = [c]
    go T           = [0, 1]
    go (p1 :+: p2) = polyadd (go p1) (go p2)
    go (p1 :-: p2) = polysub (go p1) (go p2)
    go (p1 :*: p2) = polymul (go p1) (go p2)
#endif



#ifdef FFT
instance Integral a => Num (Poly a) where
#else
instance (Eq a, Num a) => Num (Poly a) where
#endif
  p1 + p2 = poly (polyadd (toList (coeffs p1)) (toList (coeffs p2)))
  p1 - p2 = poly (polysub (toList (coeffs p1)) (toList (coeffs p2)))
  p1 * p2 = poly (polymul (toList (coeffs p1)) (toList (coeffs p2)))
  abs = undefined
  signum = undefined
  fromInteger = MkPoly . singleton . fromInteger


-- | Divides two polynomials.
--
-- Polynomial division fails if we either divide by the zero polynomial or
-- we have a non-zero remainder.
--
-- You do not need `polydiv` in the following. You may skip its implementation,
-- if you are not interested.
--
-- If we wanted to define `div` and `mod` (or `quotRem`), then we would need
-- to define a lot of other typeclass instances. Here, we do not bother to
-- implement them.
polydiv :: Integral a => PolyE a -> PolyE a -> Maybe (PolyE a)
#ifdef TEMPLATE
polydiv = error "not implemented"
#else
polydiv pe qe = 
  case q of 
    MkPoly (0 :| []) -> Nothing
    _                -> fmap toPolyE (go p)
  where
    p = fromPolyE pe
    q = fromPolyE qe

    b = leading q
    m = degree q

    go (MkPoly (0 :| [])) = Just 0
    go p = let a = leading p
               n = degree p
               u = fromPolyE (fromIntegral (a `div` b) * T ^ (n - m))
               r = p - u * q
            in if n >= m then fmap (u +) (go r) else Nothing
#endif

infixl 7 `polydiv`

#ifdef TEMPLATE
#else
-- | Converts a normalized polynomial into a `PolyE`.
toPolyE :: (Num a, Ord a) => Poly a -> PolyE a
toPolyE (MkPoly (0 :| [])) = 0
toPolyE (MkPoly cs) = 
  case pretty p of
    Nothing -> 0
    Just q  -> q
  where
    p = foldr1 (+) (zipWith (\c tp -> Const c * tp) cs (1 :| iterate (* T) T))

    -- Removes 0 * t^k and turns ... + (-c) * t^k into ... - c * t^k
    pretty (Const c1 :*: Const c2) = 
      let c = c1 * c2 
       in if c == 0 then Nothing else Just (Const c)
    pretty (Const c :*: tp)
      | c == 0    = Nothing
      | c == 1    = Just tp
      | otherwise = Just (Const c * tp)
    pretty (p :+: (Const c :*: tp)) =
      case pretty p of
        Just q   | c < 0  -> Just (q - Const (-c) * tp)
                 | c == 1 -> Just (q + tp)
                 | c > 0  -> Just (q + Const c * tp)
        Nothing  | c /= 0 -> Just (Const c * tp)
        _                 -> Nothing
    pretty _ = error "should not happen"

#endif

-- | Evaluates a polynomial at a given point using Horner's method.
horner :: Num a => Poly a -> a -> a
#ifdef TEMPLATE
horner = error "not implemented"
#else
horner p x = foldr (\c r -> c + x * r) 0 (coeffs p)
#endif

-- | Computes all divisors of an integral values.
divisors :: Integral a => a -> [a]
#ifdef TEMPLATE
divisors = error "not implemented"
#else
divisors a = nub' (xs ++ reverse ys)
  where
    (xs, ys) = unzip [(p, q) | p <- takeWhile ((<= a) . (^ 2)) [1..]
                             , let (q, r) = a `quotRem` p
                             , r == 0
                             ]

    -- Linear nub for sorted lists
    nub' []  = []
    nub' [x] = [x]
    nub' (x:y:xs)
      | x == y    = nub' (y:xs)
      | otherwise = x : nub' (y:xs)
#endif

-- | Computes the roots of a polynomial by testing all divisors of the 
-- polynomials absolute term.
roots :: Integral a => PolyE a -> [a]
#ifdef TEMPLATE
roots = error "not implemented"
#else
roots pe = let p   = fromPolyE pe
               ds  = divisors (abs (absolute p))
               ds' = map negate (reverse ds) ++ [0] ++ ds
            in filter (\x -> horner p x == 0) ds'
#endif

