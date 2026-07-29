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
#ifdef TEMPLATE
data Ratio a
#else
data Ratio a = a :% a
  deriving (Show, Eq)

infixl 7 :%
#endif


-- |':%' constructor that lets you omit the @:@ and also normalizes the 
-- representation.
(%) :: Integral a => a -> a -> Ratio a
#ifdef TEMPLATE
(%) = error "not implemented"
#else
p % q = (p `div` g) :% (q `div` g)
  where g = gcd p q
#endif

infixl 7 %


-- |Numerator of a rational number
numerator :: Ratio a -> a
#ifdef TEMPLATE
numerator = error "not implemented"
#else
numerator (p :% _) = p
#endif

-- |Denominator of a rational number
denominator :: Ratio a -> a
#ifdef TEMPLATE
denominator = error "not implemented"
#else
denominator (_ :% q) = q
#endif

-- |Adds two rational numbers
radd :: Integral a => Ratio a -> Ratio a -> Ratio a
#ifdef TEMPLATE
radd = error "not implemented"
#else
radd (p1 :% q1) (p2 :% q2) = (p1 * q2 + p2 * q1) % (q1 * q2)
#endif

-- |Subtracts two rational numbers
rsub :: Integral a => Ratio a -> Ratio a -> Ratio a
#ifdef TEMPLATE
rsub = error "not implemented"
#else
rsub (p1 :% q1) (p2 :% q2) = (p1 * q2 - p2 * q1) % (q1 * q2)
#endif

-- |Multiplies two rational numbers
rmul :: Integral a => Ratio a -> Ratio a -> Ratio a
#ifdef TEMPLATE
rmul = error "not implemented"
#else
rmul (p1 :% q1) (p2 :% q2) = (p1 * p2) :% (q1 * q2)
#endif

-- |Divides two rational numbers
rdiv :: Integral a => Ratio a -> Ratio a -> Ratio a
#ifdef TEMPLATE
rdiv = error "not implemented"
#else
rdiv (p1 :% q1) (p2 :% q2) = (p1 * q2) :% (q1 * p2)
#endif


-- |Returns the string representation with given precision.
real :: (Integral a, Show a) => Int -> Ratio a -> String
#ifdef TEMPLATE
real = error "not implemented"
#else
real k x = go 1 p ++ "." ++ go k (10 * (p `mod` q))
  where
    p = numerator x
    q = denominator x

    go 0 _ = ""
    go _ 0 = ""
    go k a = show (a `div` q) ++ go (k - 1) (10 * (a `mod` q))
#endif

