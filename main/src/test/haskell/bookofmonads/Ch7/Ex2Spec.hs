-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- Failure and Logic
module Ch7.Ex2Spec (spec) where

import Control.Applicative
import Control.Monad
import Data.Text as T
import Test.Hspec
import Text.Read

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
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a
-}

instance Monoid e => Alternative (Either' e) where
  empty :: Either' e a
  empty = Left' mempty

  (<|>) :: Either' e a -> Either' e a -> Either' e a
  Right' a <|> _ = Right' a
  _ <|> m = m

{-
How can we avoid the atypical boilerplate pattern?

if some condition
   then ...   -- keep working
   else empty

i.e. avoid the following:
-}
type Age = Int

validateAgeBoilerplate :: String -> Maybe Age
validateAgeBoilerplate s = do
  n <- readMaybe s  -- Turn the String into an Integer
  if n >= 18
    then Just n
    else Nothing

{-
Instead, we could use the `guard` function defined in Control.Monad:

guard :: Alternative m => Bool -> m ()
guard True  = pure ()
guard False = empty
-}
validateAge :: String -> Maybe Age
validateAge s = do
  n <- readMaybe s  -- Turn the String into an Integer
  guard (n >= 18)
  return n

{-
ghci
:load Ex2Spec
:reload Ex2Spec
-}
spec :: Spec
spec = do
  describe "" $ do
    it "" $ do
      1 `shouldBe` 1