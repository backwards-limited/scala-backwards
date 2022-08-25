-- {-# LANGUAGE NoImplicitPrelude #-}

-- State Monad
module Ch6.Ex1Spec (spec) where

import Control.Monad.Cont hiding (mapM, sequence, when)
import Prelude hiding (mapM, sequence)
import Test.Hspec

newtype State s a = State { runState :: s -> (a, s) }
{-
where runState is just the accessor for the new type with the signature:
runState :: State s a -> a -> (a, s)
-}

instance Functor (State s) where
  fmap f (State runState) = State (\s -> let (a, s') = runState s in (f a, s'))

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)

  (<*>) (State runStatef) (State runState) =
    State $ \s ->
      let (f, s') = runStatef s
          (a, s'') = runState s'
      in (f a, s'')

instance Monad (State s) where
  return = pure

  (>>=) (State r) amb =
    State $ \s ->
      let (a, s') = r s
      in ($ s') . runState . amb $ a

{-
DO NOT DO THE FOLLOWING

We could inspect or update the internal state directly.
For example, this is a function that updates a counter - represented as an integral state — and gives back the current value:
-}
nextValue :: State Int Int
nextValue = State (\i -> (i, i + 1))
{-
This is not a good idea for exactly the same reasons that it is not a good idea to access private methods from a module or class.
Instead, we should use the functions that State provides as an interface to build stateful computations.

The interface for State is comprised of three functions, namely (get, put, modify):
-}
get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = do
  s <- get
  put $ f s

eval :: State s a -> s -> a
eval c = fst . runState c -- Keep only the value

exec :: State s a -> s -> s
exec c = snd . runState c  -- Keep only the final state

{-
ghci
:load Ex1Spec
:reload Ex1Spec
-}
spec :: Spec
spec = do
  describe "Blah" $ do
    it "nothing to see here" $ do
      1 `shouldBe` 1