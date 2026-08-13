module Playground.Language.MonadicParser where

#ifdef TEMPLATE
#else
import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Data.Char (isPrint)

newtype Parser a = Parser { getParser :: String -> [(a, String)] }

instance Functor Parser where
  fmap f p = Parser (\s -> fmap (\(x, s') -> (f x, s')) (getParser p s))

instance Applicative Parser where
  pure x = Parser (\s -> [(x, s)])

  pf <*> px = Parser (\s -> [(f x, s'') | (f, s') <- getParser pf s
                                        , (x, s'') <- getParser px s'
                                        ])

instance Alternative Parser where
  empty = Parser (const [])

  px <|> py = Parser (\s -> getParser px s ++ getParser py s)

instance Monad Parser where
  px >>= k = Parser (\s -> [(y, s'')  | (x, s') <- getParser px s 
                                      , (y, s'') <- getParser (k x) s'
                                      ])

instance MonadPlus Parser where

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser (\s -> case s of
                            c : cs | p c -> [(c, cs)]
                            _            -> empty)

char :: Char -> Parser Char
char c = satisfy (== c)

anyChar :: Parser Char
anyChar = satisfy isPrint
#endif

