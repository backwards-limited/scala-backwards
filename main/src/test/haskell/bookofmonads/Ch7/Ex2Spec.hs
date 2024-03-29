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
Guard example for a Pythagorean triple:
The bind construct of the list monad works like a conjunction: a value has to satisfy all the conditions in the do to make an appearance in the result list.
Find all the triples of numbers from a list such that the sum of the squares of the first two equal the square of the third one.
-}
pyts :: [Integer] -> [(Integer, Integer, Integer)]
pyts ns = do
  x <- ns
  y <- ns
  z <- ns
  guard (x * x + y * y == z * z)
  return (x, y, z)

class Monad m => MonadError e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

instance MonadError e (Either e) where
  throwError = Left
  catchError (Left e) f = f e
  catchError a _ = a

instance MonadError () Maybe where
  throwError _ = Nothing
  catchError Nothing f = f ()
  catchError j _ = j

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