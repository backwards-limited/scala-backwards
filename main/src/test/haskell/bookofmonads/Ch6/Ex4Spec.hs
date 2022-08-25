-- {-# LANGUAGE NoImplicitPrelude #-}

-- Bi-, Contra-, and Profunctors
module Ch6.Ex4Spec (spec) where

import Data.Functor.Contravariant
import Test.Hspec

{-
Reminder of Functors regarding State, Reader, Writer:

fmap :: (a -> b) -> State  s a -> State  s b

fmap :: (a -> b) -> Reader r a -> Reader r b

fmap :: (a -> b) -> Writer w a -> Writer w b

Functors always operate on the type appearing in the last position of the type constructor.
-}

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

-- Taking Writer as an example, we can switch things around and instead modify the aggregated value:
mapWriter :: (v -> w) -> Writer v a -> Writer w a
mapWriter f (Writer (v, a)) = Writer (f v, a)

{-
Given fmap and mapWriter, Writer can be thought of as a functor in both type arguments.
Bifunctor is an extension of Functor composed of those types that support mapping in both positions:
-}
class Bifunctor f where
  first :: (v -> w) -> f v a -> f w a
  second :: (a -> b) -> f v a -> f v b
  -- Alternatively, map over both at the same time
  bimap :: (v -> w) -> (a -> b) -> f v a -> f w b

{-
Now for an example of a contravariant functor: a predicate i.e. a function that returns a Boolean (where Predicate is already defined in Haskell):
-}
newtype MyPredicate a = MyPredicate { runPredicate :: a -> Bool }

{-
Now if we have a function a -> b, what can we say about predicates over those types?
Well, if we have a predicate for type b, we can generate a predicate for type a by first turning the a type into a b type and then applying the Boolean function:
-}
through :: (a -> b) -> MyPredicate b -> MyPredicate a
through f (MyPredicate p) = MyPredicate (p . f)

{-
Scala Cats has a Contravariant trait. The related type class in Haskell is in the contravariant package:

class Contravariant f where
  contramap :: (a -> b) -> f b -> f a
-}

instance Contravariant MyPredicate where
  contramap = through

{-
Writer is a bifunctor - acts as a functor in both type arguments.

Reader is a profunctor - acts as a contravarant functor in its first argument,
                         acts as a functor in its second argument.
-}

class Profunctor f where
  lmap   :: (v -> w) -> f w a -> f v a
  rmap   :: (a -> b) -> f v a -> f v b
  -- Alternatively, map over both at the same time
  dimap :: (v -> w) -> (a -> b) -> f w a -> f v b

-- Here, Returns type is an example of a contravariant functor.
newtype Returns r a = R (a -> r)

instance Contravariant (Returns r) where
  contramap f (R ar) = R $ ar . f

{-
ghci
:load Ex4Spec
:reload Ex4Spec
-}
spec :: Spec
spec = do
  describe "" $ do
    it "" $ do
      1 `shouldBe` 1
