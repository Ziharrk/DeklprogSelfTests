import Data.List


data PolyE a = Const a
             | T
             | PolyE a :+: PolyE a
             | PolyE a :-: PolyE a
             | PolyE a :*: PolyE a
  deriving (Eq, Show)

infixl 6 :+:
infixl 6 :-:
infixr 7 :*:

instance Num a => Num (PolyE a) where
  (+) = (:+:)
  (-) = (:-:)
  (*) = (:*:)
  abs p = undefined
  signum p = undefined
  fromInteger = Const . fromInteger


-- TODO Implement `polyadd`, `polysub`, `polymul`

polyadd :: Integral a => [a] -> [a] -> [a]
polyadd = undefined

polysub :: Integral a => [a] -> [a] -> [a]
polysub = undefined

polymul :: Integral a => [a] -> [a] -> [a]
polymul = undefined


newtype Poly a = Poly { coeffs :: [a] }
  deriving (Eq, Show)

poly :: Integral a => [a] -> Poly a
poly = nf . Poly 

nf :: Integral a => Poly a -> Poly a
nf p = Poly (dropWhileEnd (== 0) (coeffs p))

deg :: Integral a => Poly a -> Int
deg p = length (coeffs p) - 1

leading :: Poly a -> a
leading = last . coeffs

absolute :: Poly a -> a
absolute = head . coeffs


-- TODO Implement `fromPolyE`

fromPolyE :: Integral a => PolyE a -> Poly a
fromPolyE = undefined


instance Integral a => Num (Poly a) where
  p1 + p2 = nf (Poly (polyadd (coeffs p1) (coeffs p2)))
  p1 - p2 = nf (Poly (polysub (coeffs p1) (coeffs p2)))
  p1 * p2 = nf (Poly (polymul (coeffs p1) (coeffs p2)))
  abs = undefined
  signum = undefined
  fromInteger = Poly . singleton . fromInteger


-- TODO (Optional) Implement `polydiv`

polydiv :: Integral a => Poly a -> Poly a -> Poly a
polydiv = undefined


-- TODO Implement `horner`

horner :: Num a => Poly a -> a -> a
horner = undefined


-- TODO Implement `divisors`

divisors :: Integral a => a -> [a]
divisors = undefined


-- TODO Implement `roots`

roots :: Integral a => PolyE a -> [a]
roots = undefined

