module Playground.Test.Main where

import Control.Monad (forM_)
import Test.QuickCheck

import qualified Playground.Test.SearchTree as SearchTree
import qualified Playground.Test.RE as RE

return []

allProps = concat [ SearchTree.props
                  , RE.props
                  ]

main :: IO ()
main = forM_ allProps $ \(name, prop) -> do
  putStrLn name
  quickCheckWith (stdArgs { maxSize = 18 }) prop

