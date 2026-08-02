module Playground.Test.SearchTreeSpec where

import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSize, prop)
import Test.QuickCheck

import Data.List ((\\), nub)
import Test.QuickCheck
import Playground.SearchTree (SearchTree)
import qualified Playground.SearchTree as SearchTree


fromList :: Ord a => [a] -> SearchTree a
fromList = foldr SearchTree.insert SearchTree.empty

instance (Arbitrary a, Ord a) => Arbitrary (SearchTree a) where
  arbitrary = fromList <$> arbitrary

newtype UniqueList a = UniqueList { getUniqueList :: [a] }
  deriving Show

instance (Arbitrary a, Eq a) => Arbitrary (UniqueList a) where
  arbitrary = UniqueList . nub <$> arbitrary


spec :: Spec
spec = do
  describe "SearchTree.member" $ do
    prop "Positive samples" $
      \xs -> all (`SearchTree.member` fromList xs) (xs :: [Int])

    prop "Negative samples" $
      \(UniqueList xs) (UniqueList ys) -> all (not . (`SearchTree.member` fromList xs)) (ys \\ xs :: [Int])

  describe "SearchTree.delete" $ do
    prop "insert x and delete x" $
      \xs -> let tree = foldr SearchTree.delete (fromList xs) (xs :: [Int])
              in all (not . (`SearchTree.member` tree)) xs

