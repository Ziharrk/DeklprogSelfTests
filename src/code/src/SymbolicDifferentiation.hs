{-# OPTIONS_GHC -Wno-missing-methods #-}
module SymbolicDifferentiation 
  ( Fun(..)
  , ($$)
  , derive
  ) where


-- |'Fun' represents a function of a variable 'X'.
data Fun = X          
         -- ^ A variable
         | E          
         -- ^ Euler's constant
         | Const Double 
         -- ^ Constant
         | Ln Fun     
         -- ^ Natural logarithm
         | Fun :+: Fun
         -- ^ Addition
         | Fun :-: Fun
         -- ^ Subtraction
         | Fun :*: Fun
         -- ^ Multiplication
         | Fun :/: Fun
         -- ^ Division
         | Fun :<: Fun
         -- ^ Composition
         | Fun :^: Fun
         -- ^ Exponentiation
  deriving Show


-- You can ignore the following code until '($$)'. All it does is enable you
-- to write @log (X + 2 * X)@ instead of @Ln (X :+: (Const 2 :*: X))@ with
-- a few exceptions.

infixl 6 :+:
infixl 6 :-:
infixl 7 :*:
infixl 7 :/:
infixr 8 :^:
infixr 9 :<:

instance Num Fun where
  (+) = (:+:)
  (-) = (:-:)
  (*) = (:*:)
  abs = undefined  -- requires an additional constructor
  signum = undefined  -- requires an additional constructor
  fromInteger = Const . fromInteger

instance Fractional Fun where
  (/) = (:/:)
  fromRational = Const . fromRational

instance Floating Fun where
  exp = (E :^:)
  log = Ln
  -- a lot of methods are missing here

-- Unfortunately, '(.)' and '(^)' cannot be overloaded. However, '(^)' can be
-- used with 'Fun', but @X ^ 2@ would become @X :*: X@ instead of @X :^: 2@.


($$) :: Fun -> Double -> Double
#ifdef TEMPLATE
($$) = undefined
#else
X         $$ x = x
E         $$ _ = exp 1
(Ln f)    $$ x = log (f $$ x)
(Const x) $$ _ = x
(f :+: g) $$ x = f $$ x + g $$ x
(f :*: g) $$ x = f $$ x * g $$ x
(f :-: g) $$ x = f $$ x - g $$ x
(f :/: g) $$ x = f $$ x / g $$ x
(f :<: g) $$ x = f $$ (g $$ x)
(f :^: g) $$ x = (f $$ x) ** (g $$ x)
#endif


derive :: Fun -> Fun
#ifdef TEMPLATE
derive = undefined
#else
derive X         = Const 1.0
derive E         = Const 0.0
derive (Const _) = Const 0.0
derive (Ln f)    = derive f :/: f
derive (f :+: g) = derive f :+: derive g
derive (f :-: g) = derive f :-: derive g
derive (f :*: g) = (f' :*: g) :+: (f :*: g')
  where
    f' = derive f
    g' = derive g
derive (f :/: g) = ((f' :*: g) :+: (f :*: g')) :/: (g :*: g)
  where
    f' = derive f
    g' = derive g
derive (f :<: g) = g' :*: (f' :<: g)
  where
    f' = derive f
    g' = derive g
derive (f :^: g) = derive h :*: (E :^: h)
  where h = Ln f :*: g
#endif

