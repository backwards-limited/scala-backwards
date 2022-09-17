-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- Monads for Mutability
-- Transactions e.g. TVar
module Ch8.Ex1Spec (spec) where

import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad.STM
import Data.List (lookup)
import Test.Hspec

{-
In the following piece of code, we use two of them, a counter acting as a source of identifiers and a list of names paired with those identifiers.
We'll avoid duplicates in the list of names by first checking (lookup) before modifying.

All operations in the block are treated as an atomic block that executes over a consistent view of the data.
You do not have to worry about the scenarios in which the variable counter is updated between the read and the final write
— counter always increases after a successful completion of addName
— or in which different threads lock counter and names independently and lead to a deadlock.
The runtime system will ultimately guarantee the atomicity of the transaction.
-}
addName :: TVar Integer -> TVar [(Integer, String)] -> String -> STM ()
addName counter names name = do
  i <- readTVar counter
  ns <- readTVar names
  case lookup i ns of
    Nothing -> do
      modifyTVar names ((i, name) :)
      writeTVar counter (i + 1)
    Just _ ->
      return ()
{-
Merely writing a STM computation does not lead to any mutation.
In order to execute the transaction, you need to call the atomically function:
atomically (addName "David")
-}

{-
ghci
:load Ex1Spec
:reload Ex1Spec

https://hspec.github.io/options.html
stack test --ta '-m "division by zero"'
-}
spec :: Spec
spec = do
  describe "Exception" $ do
    it "handles division by zero" $ do
      (print (5 `div` 0)) `catch` (\(e :: ArithException) -> putStrLn "Wrong number of people")
      1 `shouldBe` 1