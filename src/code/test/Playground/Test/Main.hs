module Playground.Test.Main where

import Control.Monad (forM_)
import Test.QuickCheck

import qualified Playground.Test.SearchTree as SearchTree

return []

main :: IO ()
main = forM_ (concat [SearchTree.props]) $ \(name, prop) -> do
  putStrLn name
  quickCheck prop

