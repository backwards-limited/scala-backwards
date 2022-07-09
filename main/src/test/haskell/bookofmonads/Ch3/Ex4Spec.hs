-- {-# LANGUAGE NoImplicitPrelude #-}

-- Applicatives - Tuples before Monodial Functor
module Ch3.Ex4Spec (spec) where

import Prelude hiding (Applicative, Functor, fmap, map, pure, return, (>>=), (<*>))
import Test.Hspec

nestTriple :: (a, b, c) -> (a, (b, c))
nestTriple (a, b, c) = (a, (b, c))

nestQuadruple :: (a, b, c, d) -> (a, (b, (c, d)))
nestQuadruple (a, b, c, d) = (a, (b, (c, d)))

{-
ghci
:load Ex4Spec
:reload Ex4Spec
-}
spec :: Spec
spec = do
  describe "Tuples" $ do
    it "nest triple" $ do
      nestTriple (5, 5.0, "5") `shouldBe` (5, (5.0, "5"))

    it "nest quadruple" $ do
      nestQuadruple (5, 5.0, "5", True) `shouldBe` (5, (5.0, ("5", True)))