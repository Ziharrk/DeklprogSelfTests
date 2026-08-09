module Playground.Language.Parser where

import Prelude hiding ((<*))
import Data.Char (isDigit)


-- | A parser takes a string and produces a value along with the remaining 
-- string.
type Parser a = String -> [(a, String)]

-- | Parses a string given a parser.
--
-- If there are multiple derivations, 'parse' picks the result of the first.
parse :: Parser a -> String -> a
parse p s = case filter (null . snd) (p s) of
              (x, "") : _ -> x
              _           -> error "parser failed"

-- | Returns a parser that returns a value.
succeed :: a -> Parser a
succeed x = \s -> [(x, s)]

-- | A parser that fails.
fail :: Parser a
fail = \_ -> []

-- | Parses a character if a predicate is satisfied.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p [] = []
satisfy p (c : cs)
  | p c       = [(c, cs)]
  | otherwise = []

-- | Parses a given character.
char :: Char -> Parser Char
char c = satisfy (== c)

-- | Parses any character.
anyChar :: Parser Char
anyChar = satisfy (const True)

-- | Parses a digit.
digit :: Parser Int
digit = satisfy isDigit `andThen` \c -> succeed (read (c : ""))


-- | Takes the result of a parser and continues parsing based on a the result.
andThen :: Parser a -> (a -> Parser b) -> Parser b
andThen p k = concatMap (uncurry k) . p

-- | Applies a parser zero or more times and returns a list of parsed values.
many :: Parser a -> Parser [a]
many p = \s -> case p s of
                 [] -> [([], s)]
                 rs -> concatMap (\(x, s') -> concatMap (uncurry (\xs -> succeed (x : xs))) (many p s')) rs

-- | Applies two parsers in sequences, but keeps the result of the first parser.
(<*) :: Parser a -> Parser b -> Parser a
p <* q = p `andThen` \x s -> concatMap (\(_, s') -> [(x, s')]) (q s)


-- | Parses a number.
number :: Parser Int
number = many digit `andThen` (\ds -> succeed (go ds))
  where go = foldl' (\x d -> x * 10 + d) 0

