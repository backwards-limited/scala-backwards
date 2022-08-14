-- {-# LANGUAGE NoImplicitPrelude #-}

-- Pure Reader-Writer-State Monads
module Ch6.Ex1Spec (spec) where

import Control.Monad.Cont hiding (mapM, sequence, when)
import Prelude hiding (mapM, sequence)
import Test.Hspec

newtype State s a = State { run :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State run) = State (\s -> let (a, s') = run s in (f a, s'))

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)

  (<*>) (State runf) (State run) =
    State $ \s ->
      let (f, s') = runf s
          (a, s'') = run s'
      in (f a, s'')

instance Monad (State s) where
  return = pure

  (>>=) (State r) amb =
    State $ \s ->
      let (a, s') = r s
      in ($ s') . run . amb $ a

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