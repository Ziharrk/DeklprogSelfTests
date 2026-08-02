module Playground.Test.Main where

import Control.Monad (forM_)
import Test.QuickCheck

import qualified Playground.Test.SearchTree as SearchTree
import qualified Playground.Test.Language.RegExp as RegExp

return []

allProps = concat [ SearchTree.props
                  , RegExp.props
                  ]

main :: IO ()
main = forM_ allProps $ \(name, prop) -> do
  putStrLn name
  quickCheckWith (stdArgs { maxSize = 8 }) prop

