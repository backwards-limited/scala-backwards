-- {-# LANGUAGE NoImplicitPrelude #-}

-- Utilities for monadic code
module Ch4.Ex1Spec (spec) where

import Prelude hiding (mapM, sequence)
import Test.Hspec

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ [] = return []
mapM f (x : xs) = do r <- f x
                     rs <- mapM f xs
                     return (r : rs)

-- The above "mapM" is already defined in Prelude.
-- A synonym of this where the arguments are reversed is "forM":
-- forM ["Bob", "Sue"] $ \name -> print ("Hello, " ++ name)

-- If we don't want a "monadic map" as in "mapM" due to "map" making it awkward to work with monadic functions,
-- we can instead introduce this new function (again already defined within Haskell):
sequence :: Monad m => [m a] -> m [a]
sequence []       = return []
sequence (x : xs) = do r  <- x
                       rs <- sequence xs
                       return (r : rs)

-- Once again, we may use applicative style
-- sequence (x : xs) = (:) <$> x <*> sequence xs

-- Our previous mapM is now just a composition of two pieces:
-- mapM f = sequence . map f

{-
In addition to mapM, many other list functions have monadic counterparts:
-}

zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys = sequence $ zipWith f xs ys

replicateM :: Monad m => Int -> m a -> m [a]
replicateM n = sequence . replicate n

{-
as well as:

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]

foldM performs a left fold, like foldl
foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b

In module Control.Monad.Extra, package extra
partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]

In module Control.Monad.Loops, package monad-loops
      and Control.Monad.Extra, package extra
andM, orM  :: Monad m => [m Bool] -> m Bool
anyM, allM :: Monad m => (a -> m Bool) -> [a] -> m Bool

In module Control.Monad.Loops, package monad-loops
takeWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
dropWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
firstM     :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
-}

{-
ghci
:load Ex1Spec
:reload Ex1Spec
-}
spec :: Spec
spec = do
  describe "Functor" $ do
    it "map" $ do
      map (+1) [1, 2, 3] `shouldBe` [2, 3, 4]