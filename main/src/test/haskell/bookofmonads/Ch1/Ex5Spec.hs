-- {-# LANGUAGE NoImplicitPrelude #-}

module Ch1.Ex5Spec (spec) where

import Prelude hiding (map)
import Test.Hspec

-- Our own simple Lens (to work with ADTs)
data Lens s a =
  Lens {
    get :: s -> a,
    modify :: (a -> a) -> s -> s
  }

isLetter :: Char -> Bool
isLetter c = (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')) 

type Name = String
type Age = Int

data Person = Person { name :: Name, age :: Age } deriving (Show, Eq)

ageL :: Lens Person Age
ageL = Lens {
  get = age,
  modify = \f person -> person { age = f(age person) }
}

class Functor f where
  fmap :: (a -> b) -> f a -> f b
  
{-
Looked at another way:
fmap :: Functor f => (a -> b) -> (f a -> f b)

we can say our plain old function (a -> b) and been lifted into the context of the Functor.
-}  
  
{-
ghci
:load Ex5Spec
:reload Ex5Spec
-}
spec :: Spec
spec = do
  describe "Functor" $ do
    it "fmap" $ do
      1 `shouldBe` 1