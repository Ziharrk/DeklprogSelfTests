module Playground.Language.RegExp 
  ( RegExp(..)
  , nullable
  , brzozowski
  , derivative
  , member
  ) where

-- | Regular expression
#if TEMPLATE
data RegExp
#else
data RegExp = Empty
            | Epsilon
            | Let Char
            | RegExp :|: RegExp
            | RegExp :*: RegExp
            | Kleene RegExp
  deriving (Eq, Ord)

infixl 6 :|: 
infixl 7 :*: 
#endif

#ifndef TEMPLATE
-- Pretty printing for regular expressions
instance Show RegExp where
  showsPrec _ Empty       = showString "\x2205"
  showsPrec _ Epsilon     = showString "\x03b5"
  showsPrec _ (Let c)     = showChar c
  showsPrec p (r1 :*: r2) = showParen (p > 7) (showsPrec 7 r1 . showsPrec 7 r2)
  showsPrec p (r1 :|: r2) = showParen (p > 6) (showsPrec 6 r1 . showChar '|' . showsPrec 7 r2)
  showsPrec p (Kleene r)  = showsPrec 9 r . showChar '*'

#endif

-- | @ab|c*@
re1 :: RegExp
re1 = Let 'a' :*: Let 'b' :|: Kleene (Let 'c')

-- | @(a*(bc)*)|d@
re2 :: RegExp
re2 = Kleene (Kleene (Let 'a') :*: Kleene (Let 'b' :*: Let 'c')) :|: Let 'd'


-- | Checks if the language of a given regular expression is 'nullable'. A
-- language is called nullable, if it contains the empty word.
nullable :: RegExp -> Bool
#ifdef TEMPLATE
nullable = error "not implemented"
#else
nullable Empty         = False
nullable Epsilon       = True
nullable (Kleene _)    = True
nullable (re1 :|: re2) = nullable re1 || nullable re2
nullable (re1 :*: re2) = nullable re1 && nullable re2
nullable _             = False
#endif

-- | Computes the Brzozowski derivative w.r.t. a letter.
brzozowski :: Char -> RegExp -> RegExp
#ifdef TEMPLATE
brzozowski = error "not implemented"
#else
brzozowski a Empty = Empty
brzozowski a Epsilon = Empty
brzozowski a (Let b)
  | a == b    = Epsilon
  | otherwise = Empty
brzozowski a (re1 :|: re2) = brzozowski a re1 :|: brzozowski a re2
brzozowski a (re1 :*: re2)
  | nullable re1 = re1' :*: re2 :|: re2'
  | otherwise    = re1' :*: re2
  where 
    re1' = brzozowski a re1
    re2' = brzozowski a re2
brzozowski a (Kleene re) = brzozowski a re :*: Kleene re

-- | Computes the Brzozowski derivative w.r.t. a word.
derivative :: String -> RegExp -> RegExp
derivative w re = foldl (flip brzozowski) re w
#endif

-- | Checks if a word is the language of the regular expression.
member :: String -> RegExp -> Bool
#ifdef TEMPLATE
member = error "not implemented"
#else
member w re = nullable (derivative w re)
#endif

