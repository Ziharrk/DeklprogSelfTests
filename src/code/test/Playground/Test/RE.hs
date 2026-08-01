module Playground.Test.RE where

import Data.List (nub, singleton)
import Test.QuickCheck

import Playground.RE


instance Arbitrary RE where
  arbitrary = sized $ \n -> do
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

randomWord :: RE -> Gen String
randomWord  Epsilon       = return ""
randomWord  (Let c)       = return (singleton c)
randomWord  (re1 :|: re2) = elements [re1, re2] >>= randomWord
randomWord  (re1 :*: re2) = (++) <$> randomWord re1 <*> randomWord re2
randomWord  (Kleene re)   = fmap concat (listOf (randomWord re))

-- matches :: RE -> String -> (String -> Bool) -> Bool
-- matches Epsilon        s      k = k s
-- matches (Let c)        (x:xs) k = x == c && k xs
-- matches (Let _)        []     _ = False
-- matches (re1 :|: re2)  s      k = matches re1 s k || matches re2 s k
-- matches (re1 :*: re2)  s      k = matches re1 s (\s' -> matches re2 s' k)
-- matches (Kleene re)    s      k = k s || matches re s (\s' -> s' /= s && matches (Kleene re) s' k)
--
-- accepts :: RE -> String -> Bool
-- accepts re s = matches re s null
--
-- alphabet :: RE -> [Char]
-- alphabet Epsilon       = []
-- alphabet (Let c)       = [c]
-- alphabet (re1 :|: re2) = nub (alphabet re1 ++ alphabet re2)
-- alphabet (re1 :*: re2) = nub (alphabet re1 ++ alphabet re2)
-- alphabet (Kleene re)   = alphabet re
--
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

newtype WithPositiveSample = WithPositiveSample (RE, String)
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

-- prop_brzozowski :: WithPositiveSample -> Bool
-- prop_brzozowski (WithPositiveSample (re, w)) = (w `member` re) == (accepts re w)

-- prop_notMember :: WithNegativeSample -> Bool
-- prop_notMember (WithNegativeSample (re, w)) = not (w `member` re)

return []

props :: [(String, Property)]
props = $allProperties


