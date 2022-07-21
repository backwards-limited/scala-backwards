-- {-# LANGUAGE NoImplicitPrelude #-}

-- Monad Laws
module Ch5.Ex1Spec (spec) where

import Control.Monad.Cont hiding (mapM, sequence, when)
import Prelude hiding (mapM, sequence)
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