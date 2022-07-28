-- {-# LANGUAGE NoImplicitPrelude #-}

-- Pure Reader-Writer-State Monads
module Ch6.Ex1Spec (spec) where

import Control.Monad.Cont hiding (mapM, sequence, when)
import Prelude hiding (mapM, sequence)
import Test.Hspec

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State x) = State (\s -> let (a, s') = x s in (f a, s'))

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)

  (<*>) (State f) (State a) =
    State $ \s ->
      let (f', s') = f s
          (a', s'') = a s'
      in (f' a', s'')

instance Monad (State s) where
  return = pure

  (>>=) (State a) amb =
    State $ \s ->
      let (a', s') = a s
      in ($ s') . runState . amb $ a'

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