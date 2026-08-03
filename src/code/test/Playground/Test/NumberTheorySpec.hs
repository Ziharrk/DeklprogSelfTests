module Playground.Test.NumberTheorySpec where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import qualified Playground.NumberTheory as NumberTheory

spec :: Spec
spec = do
  describe "NumberTheory.gcd" $ do
    prop "Compare against Prelude.gcd" $
      \x y -> NumberTheory.gcd (x :: Int) y == gcd x y

