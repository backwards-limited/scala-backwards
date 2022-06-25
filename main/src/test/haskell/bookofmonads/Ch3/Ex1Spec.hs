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