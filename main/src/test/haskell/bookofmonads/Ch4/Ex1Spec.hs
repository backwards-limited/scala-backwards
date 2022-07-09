-- {-# LANGUAGE NoImplicitPrelude #-}

-- Utilities for monadic code
module Ch4.Ex1Spec (spec) where

import Test.Hspec



{-
ghci
:load Ex1Spec
:reload Ex1Spec
-}
spec :: Spec
spec = do
  describe "Functor" $ do
    it "map" $ do
      map (+1) [1, 2, 3] `shouldBe` [2, 3, 4]

  describe "Monadic action" $ do
    it "will do nothing when mapped over" $ do
      map (\name -> print ("Hello, " ++ name)) ["Bob", "Sue"] `shouldBe` []
