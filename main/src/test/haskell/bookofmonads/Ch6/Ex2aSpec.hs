-- {-# LANGUAGE NoImplicitPrelude #-}

-- My Reader Monad
module Ch6.Ex2aSpec (spec) where

import Control.Monad.Cont hiding (mapM, sequence, when)
import Prelude hiding (mapM, sequence)
import Test.Hspec

newtype Reader r a = Reader { runReader :: r -> a }
{-
where runReader is just the accessor for the new type with the signature:
runReader :: Reader r a -> a
-}

instance Functor (Reader r) where
  fmap f (Reader runReader) = Reader $ f . runReader

instance Applicative (Reader r) where
  pure a = Reader $ \_ -> a

  (Reader runReaderf) <*> (Reader runReader) =
    Reader $ \r -> runReaderf r $ runReader r

instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  ma >>= amb = Reader $ \r -> runReader (amb (runReader ma r)) r

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = Reader f

local :: (r -> r) -> Reader r a -> Reader r a
local f m = Reader $ runReader m . f

data Account =
  Account {
    name :: String,
    age :: Int
  } deriving (Eq, Show)

computation :: Reader Account (Maybe String)
computation =
  asks name >>= \n -> asks age >>= \a -> if a > 18 then return (Just n) else return Nothing

-- The above equates to:
computationA :: Reader Account (Maybe String)
computationA =
  Reader name >>= \n -> Reader age >>= \a -> if a > 18 then return (Just n) else return Nothing

-- One element at a time:
accountName :: Account -> String
accountName account = name account

-- Going point free, highlighting that name is just a function:
accountName' :: Account -> String
accountName' = name

accountNameR :: Reader Account String
accountNameR = Reader name -- Reader (r -> a) i.e. Reader (Account -> String)

{-
computation = do
  n <- asks name
  a <- asks age
  if a > 18
    then return (Just n)
  else
    return Nothing
-}

{-
ghci
:load Ex2aSpec
:reload Ex2aSpec
-}
spec :: Spec
spec = do
  describe "Reader" $ do
    it "run first example" $ do
      (runReader computation $ Account "Scooby" 21) `shouldBe` (Just "Scooby")

    it "run second example" $ do
      (runReader computation $ Account "Scooby" 12) `shouldBe` Nothing

    it "one element at a time" $ do
      (runReader accountNameR $ Account "Bob" 5) `shouldBe` "Bob"