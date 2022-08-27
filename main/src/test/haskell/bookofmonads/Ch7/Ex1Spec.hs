-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- Failure and Logic
module Ch7.Ex1Spec (spec) where

import Data.Text as T
import Test.Hspec

(<|>) :: Maybe a -> Maybe a -> Maybe a
Just x  <|> _     = Just x
Nothing <|> other = other

newtype Name = Name String deriving (Show, Eq)

validateNameEnglish :: String -> Maybe Name
validateNameEnglish s =
  if "Mr" `T.isInfixOf` (T.pack s) then Just $ Name s else Nothing

validateNameDutch :: String -> Maybe Name
validateNameDutch s =
  if "van" `T.isInfixOf` (T.pack s) then Just $ Name s else Nothing

validateName :: String -> Maybe Name
validateName s =
  validateNameEnglish s <|> validateNameDutch s

-- Let's define our own Either
data Either' e r = Left' e | Right' r

instance Functor (Either' e) where
  fmap _ (Left' e) = Left' e
  fmap f (Right' a) = Right' $ f a

instance Applicative (Either' e) where
  pure = Right'

  Right' f <*> Right' a = Right' $ f a
  _ <*> Left' e = Left' e
  Left' e <*> _ = Left' e

instance Monad (Either' e) where
  return = pure

  Left' e  >>= _ = Left' e
  Right' a >>= f = f a

{-
ghci
:load Ex1Spec
:reload Ex1Spec
-}
spec :: Spec
spec = do
  describe "Simple Failure" $ do
    it "performs validation successfully" $ do
      validateNameEnglish "Mr Scooby" `shouldBe` (Just $ Name "Mr Scooby")

    it "performs validation and fails" $ do
      validateNameEnglish "Scooby" `shouldBe` Nothing

    it "performs validation successfully" $ do
      validateNameDutch "Scooby van Doo" `shouldBe` (Just $ Name "Scooby van Doo")

    it "performs validation successfully" $ do
      validateNameDutch "Scooby Doo" `shouldBe` Nothing

  describe "Simple Failure combinator" $ do
    it "performs validation successfully" $ do
      validateName "Scooby van Doo" `shouldBe` (Just $ Name "Scooby van Doo")