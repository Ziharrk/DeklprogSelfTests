-- TODO come up with better names for the typeclasses and -families
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

import Data.Kind
import GHC.TypeLits

data D a = D a a

-- Hier könnten deine Num-, Fractional- und Floating-Typklasseninstanzen
-- stehen!
instance Num a => Num (D a) where
  D x1 d1 + D x2 d2 = D (x1 + x2) (d1 + d2)
  -- ...

instance Fractional a => Fractional (D a) where
  -- ...

instance Floating a => Floating (D a) where
  exp (D x d) = D (exp x) (d * exp x)  -- Kettenregel und exp'(x) = exp(x)
  -- ...


class Derivable (n :: Nat) b where
  type Derivative n b
  derive :: (Derivative n b -> Derivative n b) -> b -> b

instance (Wrap (NatToPeano n) b, Unwrap (DK (NatToPeano n) b) b) => Derivable n b where
  type Derivative n b = DK (NatToPeano n) b
  derive f x = unwrap (f (wrap @(NatToPeano n) x))

data Peano = Z | S Peano

type family NatToPeano (n :: Nat) where
  NatToPeano 0 = Z
  NatToPeano k = S (NatToPeano (k - 1))

type family DK p a where
  DK Z     a = a
  DK (S k) a = D (DK k a)

class Wrap n t where
  wrap :: t -> DK n t

instance Wrap Z t where
  wrap x = x

instance (Wrap n t, Num (DK n t)) => Wrap (S n) t where
  wrap x = D (wrap @n x) 1

class Unwrap a b | a -> b where
  unwrap :: a -> b

instance (Num a) => Unwrap a a where
  unwrap x = x

instance (Unwrap a b, Num a) => Unwrap (D a) b where
  unwrap (D _ d) = unwrap d


main :: IO ()
main = print (derive @1 (\x -> exp (x + x + x)) (1.0 :: Double))

