module Rational 
  ( Ratio
  , (%)
  , numerator
  , denominator
  , radd
  , rsub
  , rmul
  , rdiv
  , real
  ) where


-- |Rational number.
#if TEMPLATE
data Ratio a
#else
data Ratio a = a :% a
  deriving (Show, Eq)

infixl 7 :%
#endif


-- |':%' constructor that lets you omit the @:@ and also normalizes the 
-- representation.
(%) :: Integral a => a -> a -> Ratio a
#if TEMPLATE
(%) = undefined
#else
p % q = (p `div` g) :% (q `div` g)
  where g = gcd p q
#endif

infixl 7 %


-- |Numerator of a rational number
numerator :: Ratio a -> a
#if TEMPLATE
numerator = undefined
#else
numerator (p :% _) = p
#endif

-- |Denominator of a rational number
denominator :: Ratio a -> a
#if TEMPLATE
denominator = undefined
#else
denominator (_ :% q) = q
#endif

-- |Adds two rational numbers
radd :: Integral a => Ratio a -> Ratio a -> Ratio a
#if TEMPLATE
radd = undefined
#else
radd (p1 :% q1) (p2 :% q2) = (p1 * q2 + p2 * q1) % (q1 * q2)
#endif

-- |Subtracts two rational numbers
rsub :: Integral a => Ratio a -> Ratio a -> Ratio a
#if TEMPLATE
rsub = undefined
#else
rsub (p1 :% q1) (p2 :% q2) = (p1 * q2 - p2 * q1) % (q1 * q2)
#endif

-- |Multiplies two rational numbers
rmul :: Integral a => Ratio a -> Ratio a -> Ratio a
#if TEMPLATE
rmul = undefined
#else
rmul (p1 :% q1) (p2 :% q2) = (p1 * p2) :% (q1 * q2)
#endif

-- |Divides two rational numbers
rdiv :: Integral a => Ratio a -> Ratio a -> Ratio a
#if TEMPLATE
rdiv = undefined
#else
rdiv (p1 :% q1) (p2 :% q2) = (p1 * q2) :% (q1 * p2)
#endif


-- |Returns the string representation with given precision.
real :: (Integral a, Show a) => Int -> Ratio a -> String
#if TEMPLATE
real = undefined
#else
real k x = go 1 p ++ "." ++ go k (10 * (p `mod` q))
  where
    p = numerator x
    q = denominator x

    go 0 _ = ""
    go _ 0 = ""
    go k a = show (a `div` q) ++ go (k - 1) (10 * (a `mod` q))
#endif

