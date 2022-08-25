-- {-# LANGUAGE NoImplicitPrelude #-}

-- Reader Monad
module Ch6.Ex2Spec (spec) where

import Control.Monad.Cont hiding (mapM, sequence, when)
import Prelude hiding (mapM, sequence)
import Test.Hspec

-- Our own simple Reader implementation

{-
Ideally a really simple implemetation would use a type alias - but we would have issues defining type class instance so cannot do:

type Reader r a = r -> a

instance Monad (Reader r) where
  return a = \_ -> a

  ra >>= farb = \env -> farb (ra env) env
-}

-- So let's use newtype
newtype Reader r a = Reader { runReader :: r -> a }
{-
where runReader is just the accessor for the new type with the signature:
runReader :: Reader r a -> a
-}

instance Functor (Reader r) where
  -- fmap f (Reader runReader) = Reader $ \r -> f $ runReader r
  -- Simplified to point free:
  fmap f (Reader runReader) = Reader $ f . runReader

instance Applicative (Reader r) where
  pure a = Reader $ \_ -> a

  (Reader runReaderf) <*> (Reader runReader) =
    Reader $ \r -> runReaderf r $ runReader r

instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  ma >>= amb =
    Reader $ \r -> let a  = runReader ma r
                       mb = amb a
                   in runReader mb r
  {-
  Or shorter (though hard to understand):
  (Reader ra) >>= arb =
    Reader $ \r -> ($ r) . runReader . arb $ ra r
  -}

{-
ghci
:load Ex2Spec
:reload Ex2Spec
-}
spec :: Spec
spec = do
  describe "Blah" $ do
    it "nothing to see here" $ do
      1 `shouldBe` 1