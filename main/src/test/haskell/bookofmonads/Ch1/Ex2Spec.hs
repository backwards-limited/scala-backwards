-- {-# LANGUAGE NoImplicitPrelude #-}

module Ch1.Ex2Spec (spec) where

import Prelude hiding (concat, length, (++), map)
import Test.Hspec

length :: [a] -> Integer
length []       = 0
length (_ : xs) = 1 + length xs

(++) :: [a] -> [a] -> [a]
(++) [] ys        = ys
(++) (x : xs) ys  = x : (xs ++ ys) 

map :: (a -> b) -> [a] -> [b]
map f (x : xs) = f x : map f xs
map _ _ = []

singleton :: a -> [a]
singleton x = [x] -- or x : []

concat :: [[a]] -> [a]
concat []     = []
concat (x : xs) = x ++ concat xs
-- Or
-- concat = foldr (++) []

{-
ghci
:load Ex2Spec
:reload Ex2Spec
-}
spec :: Spec
spec = do
  describe "List" $ do
    it "length of empty list" $ do
      length [] `shouldBe` 0

    it "length of list" $ do
      length ["a", "b"] `shouldBe` 2
      
    it "concatenate lists" $ do
      ["a", "b"] ++ ["c", "d"] `shouldBe` ["a", "b", "c", "d"]
      
    it "map over" $ do
      map (\x -> x ++ "*") ["a", "b"] `shouldBe` ["a*", "b*"]       