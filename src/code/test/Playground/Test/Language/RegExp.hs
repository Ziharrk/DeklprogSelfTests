module Playground.Test.Language.RegExp where

import Data.List (nub, singleton)
import Test.QuickCheck

import Playground.Language.RegExp


instance Arbitrary RegExp where
  arbitrary = sized $ \n -> do
    -- the weights are arbitrarily chosen to prevent the generation of
    -- random words from exploding
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

randomWord :: RegExp -> Gen String
randomWord  Empty         = error "language is empty"
randomWord  Epsilon       = return ""
randomWord  (Let c)       = return (singleton c)
randomWord  (re1 :|: re2) = elements [re1, re2] >>= randomWord
randomWord  (re1 :*: re2) = (++) <$> randomWord re1 <*> randomWord re2
randomWord  (Kleene re)   = fmap concat (listOf (randomWord re))


-- -- Rejection sampling takes too long
-- -- Alternatively, construct complement DFA and search for words
-- randomNonWord :: RE -> Gen String
-- randomNonWord re = go
--   where
--     alph = if null (alphabet re) then ['a'..'z'] else alphabet re
--     go = do
--       n <- choose (0, 10)
--       s <- vectorOf n (elements alph)
--       if accepts re s then go else return s

newtype WithPositiveSample = WithPositiveSample (RegExp, String)
  deriving Show

instance Arbitrary WithPositiveSample where
  arbitrary = do
    re <- arbitrary
    w <- randomWord re
    return (WithPositiveSample (re, w))

-- newtype WithNegativeSample = WithNegativeSample (RE, String)
--   deriving Show
--
-- instance Arbitrary WithNegativeSample where
--   arbitrary = do
--     re <- arbitrary
--     w <- randomNonWord re
--     return (WithNegativeSample (re, w))


prop_member :: WithPositiveSample -> Bool
prop_member (WithPositiveSample (re, w)) = w `member` re

-- prop_notMember :: WithNegativeSample -> Bool
-- prop_notMember (WithNegativeSample (re, w)) = not (w `member` re)

-- prop_inverse :: WithPositiveSample -> Bool
-- prop_inverse (WithPositiveSample (re, w)) = member w (stateElim (compile re))

return []

props :: [(String, Property)]
props = $allProperties


