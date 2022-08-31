-- {-# LANGUAGE NoImplicitPrelude #-}

-- Writer Monad
module Ch6.Ex3aSpec (spec) where

import Test.Hspec

newtype Writer w a = Writer { runWriter :: (w, a) }
{-
where runWriter is just the accessor for the new type with the signature:
runWriter :: Writer w a -> (w, a)
-}

instance Functor (Writer w) where
  fmap :: (a -> b) -> Writer w a -> Writer w b
  fmap f (Writer (w, x)) = Writer (w, f x)

instance Monoid w => Applicative (Writer w) where
  pure x = Writer (mempty, x)

  Writer (w, f) <*> Writer (w', x) = Writer (w <> w', f x)

instance Monoid w => Monad (Writer w) where
  return = pure

  Writer (w1, x) >>= f =
    let Writer (w2, y) = f x
    in Writer (w1 `mappend` w2, y)

tell :: w -> Writer w ()
tell w = Writer (w, ())

stuff :: Writer [Int] String
stuff =
  tell [1 .. 3] >>= \_ -> tell [3 .. 5] >>= \_ -> return "foo"

{-
ghci
:load Ex3aSpec
:reload Ex3aSpec
-}
spec :: Spec
spec = do
  describe "Writer" $ do
    it "run" $ do
      runWriter stuff `shouldBe` ([1, 2, 3, 3, 4, 5], "foo")
