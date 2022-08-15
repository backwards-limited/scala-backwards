-- {-# LANGUAGE NoImplicitPrelude #-}

-- Reader Monad
module Ch6.Ex2Spec (spec) where

import Control.Monad.Cont hiding (mapM, sequence, when)
import Prelude hiding (mapM, sequence)
import Test.Hspec

{-
ghci
:load Ex2Spec
:reload Ex2Spec
-}
spec :: Spec
spec = do
  describe "Blah" $ do
    it "nothing to see here" $ do
      1 `shouldBe` 1