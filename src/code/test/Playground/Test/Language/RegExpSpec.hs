module Playground.Test.Language.RegExpSpec where

import Data.List (singleton)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSize, prop)
import Test.QuickCheck

import Playground.Language.RegExp (RegExp(..))
import qualified Playground.Language.RegExp as RegExp
import qualified Playground.Language.FA as FA


instance Arbitrary RegExp where
  arbitrary = sized $ \n -> do
    -- We use Kleene sparingly since it allows random words to grow quickly
    -- (see 'randomWord').
    f <- frequency [(1, return Kleene), (9, return id)]
    case n of
      0 -> return Epsilon
      1 -> fmap (f . Let) (elements ['a'..'z'])
      _ -> do
        k <- chooseInt (1, n)
        l <- resize k arbitrary
        r <- resize (n - k) arbitrary
        op <- elements [(:|:), (:*:)]
        return (f (op l r))


-- | Generates a random word.
randomWord :: RegExp -> Gen String
randomWord  Empty         = error "language is empty"
randomWord  Epsilon       = return ""
randomWord  (Let c)       = return (singleton c)
randomWord  (re1 :|: re2) = elements [re1, re2] >>= randomWord
randomWord  (re1 :*: re2) = (++) <$> randomWord re1 <*> randomWord re2
randomWord  (Kleene re)   = do
  k <- chooseInt (0, 3)
  fmap concat (vectorOf k (randomWord re))  -- do not want these to explode


newtype WithPositiveSample = WithPositiveSample (RegExp, String)
  deriving Show

instance Arbitrary WithPositiveSample where
  arbitrary = do
    re <- arbitrary
    w <- randomWord re
    return (WithPositiveSample (re, w))


-- TODO Rejection sampling with regular expressions is cumbersome. Once the
--      complement of a DFA is implemented, we may traverse the DFA to find
--      some words with random walks.

-- newtype WithNegativeSample = WithNegativeSample (RE, String)
--   deriving Show
--
-- instance Arbitrary WithNegativeSample where
--   arbitrary = do
--     re <- arbitrary
--     w <- randomNonWord re
--     return (WithNegativeSample (re, w))


spec :: Spec
spec = do
  describe "RegExp.member" $ do
    prop "Positive samples" $
      \(WithPositiveSample (re, w)) -> w `RegExp.member` re

    prop "FA.member and RegExp.member are equal on random samples" $
      \w re -> RegExp.member w re == FA.member w (FA.compile re)

