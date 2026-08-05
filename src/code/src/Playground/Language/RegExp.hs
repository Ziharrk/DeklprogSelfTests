module Playground.Language.RegExp 
  ( RegExp(..)
  , nullable
  , brzozowski
  , derivative
  , member
  ) where

#ifndef TEMPLATE
import Data.String (IsString(..))
#endif

-- | Regular expression
--
-- (In a different challenges we want to use RegExp as keys for Map. Thus, we
-- derive Ord here.)
#ifdef TEMPLATE
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

-- "'IsString' is used in combination with the language extension 
-- 'OverloadedStrings' language extension to convert the literals to different 
-- string types."
--
-- See https://hackage-content.haskell.org/package/base/docs/Data-String.html#t:IsString
--
-- In our case, this allows us to @"abc"@ instead of 
-- @Let 'a' :*: Let 'b' :*: Let 'c'@. If we had a parser, we could have used it
-- instead.
instance IsString RegExp where
  fromString "" = Epsilon
  fromString cs = foldr1 (:*:) (map Let cs)
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
#endif

-- | Computes the Brzozowski derivative w.r.t. a word.
derivative :: String -> RegExp -> RegExp
#ifdef TEMPLATE
derivative = error "not implemented"
#else
derivative w re = foldl (flip brzozowski) re w
#endif

-- | Checks if a word is the language of the regular expression.
member :: String -> RegExp -> Bool
#ifdef TEMPLATE
member = error "not implemented"
#else
member w re = nullable (derivative w re)
#endif

