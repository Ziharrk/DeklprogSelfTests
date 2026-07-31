module Playground.Test.SearchTree (props) where

import Data.List ((\\), nub)
import Test.QuickCheck
import Playground.SearchTree


fromList :: Ord a => [a] -> SearchTree a
fromList = foldr insert empty

instance (Arbitrary a, Ord a) => Arbitrary (SearchTree a) where
  arbitrary = fromList <$> arbitrary

newtype UniqueList a = UniqueList { getUniqueList :: [a] }
  deriving Show

instance (Arbitrary a, Eq a) => Arbitrary (UniqueList a) where
  arbitrary = UniqueList . nub <$> arbitrary


prop_member :: [Int] -> Bool
prop_member xs = all (`member` fromList xs) xs

prop_notMember :: UniqueList Int -> UniqueList Int -> Bool
prop_notMember (UniqueList xs) (UniqueList ys) = all (not . (`member` fromList xs)) (ys \\ xs)

prop_delete :: [Int] -> Bool
prop_delete xs = all (not . (`member` tree)) xs
  where tree = foldr delete (fromList xs) xs


return []

props :: [(String, Property)]
props = $allProperties

