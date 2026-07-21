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


polyadd :: Integral a => [a] -> [a] -> [a]
polyadd []     ys     = ys
polyadd xs     []     = xs
polyadd (x:xs) (y:ys) = x + y : polyadd xs ys

polysub :: Integral a => [a] -> [a] -> [a]
polysub []     ys     = map negate ys
polysub xs     []     = xs
polysub (x:xs) (y:ys) = x - y : polysub xs ys

polymul :: Integral a => [a] -> [a] -> [a]
polymul []     _  = []
polymul _      [] = []
polymul (a:as) bs = polyadd (map (a *) bs) (0 : polymul as bs)


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


fromPolyE :: Integral a => PolyE a -> Poly a
fromPolyE p = nf (Poly (go p))
  where
    go (Const c)   = [c]
    go T           = [0, 1]
    go (p1 :+: p2) = polyadd (go p1) (go p2)
    go (p1 :-: p2) = polysub (go p1) (go p2)
    go (p1 :*: p2) = polymul (go p1) (go p2)


instance Integral a => Num (Poly a) where
  p1 + p2 = nf (Poly (polyadd (coeffs p1) (coeffs p2)))
  p1 - p2 = nf (Poly (polysub (coeffs p1) (coeffs p2)))
  p1 * p2 = nf (Poly (polymul (coeffs p1) (coeffs p2)))
  abs = undefined
  signum = undefined
  fromInteger = Poly . singleton . fromInteger


polydiv :: Integral a => Poly a -> Poly a -> Poly a
polydiv p q = go p
  where
    b = leading q
    m = deg q

    go (Poly []) = Poly []
    go p = let a = leading p
               f = fromPolyE (fromIntegral (a `div` b) * T ^ (deg p - m))
               g = nf (p - f * q)
            in f + go g


horner :: Num a => Poly a -> a -> a
horner p x = foldr (\c r -> c + x * r) 0 (coeffs p)


divisors :: Integral a => a -> [a]
divisors a = let (xs, ys) = unzip [(p, q) | p <- takeWhile ((<= a) . (^ 2)) [1..]
                                          , let (q, r) = a `quotRem` p
                                          , r == 0
                                          ]
              in xs ++ reverse ys


roots :: Integral a => PolyE a -> [a]
roots pe = let p   = fromPolyE pe
               ds  = divisors (abs (absolute p))
               ds' = map negate (reverse ds) ++ [0] ++ ds
            in filter (\x -> horner p x == 0) ds'

