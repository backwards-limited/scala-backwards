-- {-# LANGUAGE NoImplicitPrelude #-}

-- Lifting pure functions
module Ch3.Ex1Spec (spec) where

import Test.Hspec

plus :: Maybe Int -> Maybe Int -> Maybe Int
plus x y = do
  a <- x
  b <- y
  return (a + b)

lift2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
lift2 f x y =
  do
    a <- x
    b <- y
    return (f a b)

simplePlus :: Int -> Int -> Int
simplePlus = (+)

{-
After lift2, we could have lift3, lift4.... liftN:

liftN :: Monad m => (a1 -> a2 -> ... -> aN -> r) -> m a1 -> m a2 -> ... -> m aN -> m r
liftN f x1 x2 ... xN = do
  a1 <- x1
  a2 <- x2
  ...
  aN <- xN
  return (f a1 a2 ... aN)
  
In the next exercise 1b, we'll code "ap" which saves us have to write map, map2, ... mapN.  
-}

{-
ghci
:load Ex1Spec
:reload Ex1Spec
-}
spec :: Spec
spec = do
  describe "Monad lifting" $ do
    it "is better than Functor lifting" $ do
      Just 2 `plus` Just 3 `shouldBe` Just 5
      
    it "is much better than Functor lifting" $ do
      lift2 simplePlus (Just 2) (Just 3) `shouldBe` Just 5
      
    it "and again is much better than Functor lifting" $ do
      lift2 (+) (Just 2) (Just 3) `shouldBe` Just 5    