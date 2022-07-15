-- {-# LANGUAGE NoImplicitPrelude #-}

-- Utilities for monadic code
module Ch4.Ex1Spec (spec) where

import Control.Monad.Cont hiding (mapM, sequence, when)
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
A note on monadic actions with only side effects e.g.
put :: s -> State s ()

The following are all equivalent, but the last is preferred:

do
  () <- put newValue

do
  _ <- put newValue

do
  put newValue
-}

-- Because of unit, Haskell in certain situations, will issue a warning e.g. a warning will be given for:
example1 :: IO [()]
example1 = forM ["Alejandro", "Elena"] $ \name -> print ("Hello, " ++ name)

{-
To avoid any warning, we can use "void" declared as:

void :: Functor m => m a -> m ()
void = fmap (\_ -> ())

now we can code:
-}
example2 :: IO ()
example2 = void $ forM ["Alejandro", "Elena"]
                $ \name -> print ("Hello, " ++ name)

{-
The idiom leads to a lot of "_" equivalent functions e.g.

mapM_ :: Monad m => (a -> m b) -> m a -> m ()

zipWithM_ :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m ()

etc.

One advantage of the empty tuple is the ease of constructing one.
If you want to create a monadic action m (), you can just use return (), regardless of the monad.
-}

{-
In imperative languages, you usually have conditionals where one of the branches may not be present.
We can provide a similar abstraction for monadic contexts by using the empty tuple as a dummy value:
-}
when :: Monad m => Bool -> m () -> m ()
when cond action = if cond then action else return ()

{-
Note, the above is already defined in: Control.Monad.Cont.when
And alternative definition:

when :: Monad m => Bool -> m () -> m ()
when True  action = action
when False _      = return ()
-}

unless :: Monad m => Bool -> m () -> m ()
unless cond = when (not cond)

{-
And we can have the condition itself within a monadic context as again defined in Control.Monad.Extra e.g.
-}

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond true false = do c <- cond
                         if c then true else false

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