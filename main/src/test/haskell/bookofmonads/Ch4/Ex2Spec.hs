-- {-# LANGUAGE NoImplicitPrelude #-}

-- Traversable
module Ch4.Ex2Spec (spec) where

import Control.Monad hiding (mapM)
import Prelude hiding (map, mapM, sequence)
import Test.Hspec

-- Pure:
map :: (a -> b) -> [a] -> [b]
map f []        = []
map f (x : xs)  = f x : map f xs

-- Monadic (equivalent):
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f [] = pure []
mapM f (x : xs)= (:) <$> f x <*> mapM f xs

-- and alternative implementation:
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = pure []
mapM' f (x : xs) = liftM2 (:) (f x) (mapM' f xs)

{-
ghci
:load Ex2Spec
:reload Ex2Spec
-}
spec :: Spec
spec = do
  describe "Traversable" $ do
    it "xxxxx" $ do
      1 `shouldBe` 1