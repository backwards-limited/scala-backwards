-- {-# LANGUAGE NoImplicitPrelude #-}

-- Monoidal Functor
module Ch3.Ex5Spec (spec) where

import Prelude hiding (Applicative, map, pure, (>>=), (<*>), (**))
import Test.Hspec

class Functor f => Monoidal f where
  unit :: f ()

  (**) :: f a -> f b -> f (a, b) -- tupled

instance Monoidal Maybe where
  unit = Just ()

  fa ** fb = do
    a <- fa
    b <- fb
    return (a, b)

{-
Amazingly, applicative and monoidal functors define the same notion!
We can see it by providing an implementation of Applicative in terms of Monoidal and vice versa:
-}
pure :: Monoidal f => a -> f a
pure x = fmap (\_ -> x) unit

(<*>) :: Monoidal f => f (a -> b) -> f a -> f b
f <*> x = fmap (\(g, y) -> g y) (f ** x)

{-
Could redefine in the other direction:

unit :: Applicative f => f ()
unit = pure ()

(**) :: Applicative f => f a -> f b -> f (a, b)
(**) a b = (,) <$> a <*> b
-}

{-
ghci
:load Ex5Spec
:reload Ex5Spec
-}
spec :: Spec
spec = do
  describe "Monoidal Functor" $ do
    it "unit for Maybe" $ do
      1 `shouldBe` 1

    it "tupled for Maybe" $ do
      1 `shouldBe` 1
