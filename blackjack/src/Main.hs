module Main where

import BlackJack (benchmark, simpleStrategy)

main :: IO ()
main = do
  avg <- benchmark simpleStrategy
  print avg

