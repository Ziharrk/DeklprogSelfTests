module Playground.Test.Language.FASpec where

import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSize, prop)
import Test.QuickCheck

import Playground.Language.RegExp (RegExp(..))
import qualified Playground.Language.RegExp as RegExp
import qualified Playground.Language.FA as FA
import Playground.Test.Language.RegExpSpec (WithPositiveSample(..))


spec :: Spec
spec = do
  describe "FA.member" $ do
    prop "via FA.compile" $
      \(WithPositiveSample (re, w)) -> w `FA.member` FA.compile re

  describe "FA.stateElim" $ do
    modifyMaxSize (const 20) $
      prop "Inverse of compile" $
        \(WithPositiveSample (re, w)) -> w `RegExp.member` FA.stateElim (FA.compile re)

