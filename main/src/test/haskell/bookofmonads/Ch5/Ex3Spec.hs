-- {-# LANGUAGE NoImplicitPrelude #-}

-- Monad Laws - What we call laws, mathematicians call theorems.
module Ch5.Ex3Spec (spec) where

import Control.Monad.Cont hiding (mapM, sequence, when)
import Prelude hiding (mapM, sequence)
import Test.Hspec

{-
ghci
:load Ex3Spec
:reload Ex3Spec
-}
spec :: Spec
spec = do
  describe "Monad" $ do
    it "nothing to see here" $ do
      1 `shouldBe` 1