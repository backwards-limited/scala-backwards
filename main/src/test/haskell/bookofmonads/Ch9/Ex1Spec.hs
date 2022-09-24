-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- Resource management
module Ch9.Ex1Spec (spec) where

import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad.STM
import Data.List (lookup)
import Test.Hspec

{-
            acquire   release        use
bracket ::  IO r ->   (r -> IO b) -> (r -> IO a) -> IO a

So we can write:
bracket acquire release $ \r -> do

The support for resource management that bracket provides is highly generic.
Many other libraries give specific versions of that function targeted to a single use case.
e.g. opening, working with, and closing files:

withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile fp mode = bracket (openFile fp mode) hClose

Things start to get a tad awkward with multiple resources
e.g. you want to process some input file and write the results back to an output file:

withFile inFile ReadMode $ \inHandle ->
  withFile outFile WriteMode $ \outHandle ->
    doWork inHandle outHandle

We can rejig the above to "run" "managed resources":

runManaged $ do
  inHandle  <- managed (withFile inFile ReadMode)
  outHandle <- managed (withFile outFile WriteMode)
  liftIO $ doWork inHandle outHandle

Notice the call to liftIO in the code above.
Computations that work with resources typically live in the IO monad.
In contrast, the do block above lives in the Managed monad.
How do we reconcile these two worlds? The answer is this function, liftIO:

liftIO :: IO a -> Managed a

With liftIO, we can treat any IO computation as an operation inside of Managed.
-}

{-
The general concept of "continuation"
-}
type Cont r a = (a -> r) -> r

return :: a -> (a -> r) -> r
return x = \k -> k x

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
      1 `shouldBe` 1