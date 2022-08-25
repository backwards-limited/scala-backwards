-- {-# LANGUAGE NoImplicitPrelude #-}

-- Writer Monad
module Ch6.Ex3Spec (spec) where

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

newtype Sum a = Sum { get :: a } deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Semigroup (Sum a) where
  Sum x <> Sum y = Sum $ x + y

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend = (<>)

demo :: Writer (Sum Int) String
demo = do tell (Sum 3)
          tell (Sum 4)
          return "seven"

{-
Brief note on RWS (included in both transformers and Cats, and discussed in later exercises).
The interface of the RWS monad is just the aggregation of the interfaces of Reader, Writer, and State.
In addition, it provides three functions to embed computations from each of the monads that make up the mix:

reader :: Monoid w => (r ->  a)     -> RWS r w s a

writer ::                   (a, w)  -> RWS r w s a

state  :: Monoid w => (s -> (a, s)) -> RWS r w s a
-}

{-
ghci
:load Ex3Spec
:reload Ex3Spec
-}
spec :: Spec
spec = do
  describe "Writer" $ do
    it "will tell" $ do
      runWriter demo `shouldBe` (Sum 7, "seven")
