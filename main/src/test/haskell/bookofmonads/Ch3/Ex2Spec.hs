-- {-# LANGUAGE NoImplicitPrelude #-}

-- Applicatives
module Ch3.Ex2Spec (spec) where

import Prelude hiding (Applicative, Functor, fmap, map, pure, return, (>>=), (<*>))
import Test.Hspec

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure :: a -> f a
  
  (<*>) :: f (a -> b) -> f a -> f b

fmap_ :: Applicative f => (a -> b) -> f a -> f b
fmap_ f a = pure f <*> a

newtype ZipList a =
  ZipList { getZipList :: [a] } deriving (Show, Eq)
  
instance Functor [] where
  fmap f [] =
    []
  
  fmap f (x : xs) =
    f(x) : fmap f xs
  
instance Functor ZipList where
  fmap f (ZipList xs) =
    ZipList $ fmap f xs 
  
instance Applicative ZipList where
  pure a =
    ZipList (repeat a)
  
  ZipList fabs <*> ZipList fas =
    ZipList $ zipWith ($) fabs fas -- ZipList (zipWith (\ab a -> ab a) fabs fas)

{-
A pattern emerges, which will fomulate the "applicative style".

Each time we want to use a pure function in a monadic (or applicative) context, we use:
fmap f x1 `ap` x2 `ap` ... `ap` xN

by introducing symbols we get:
f <$> x1 <*> x2 <*> ... <*> xN

Now instead of the following do block:
do x <- [1,2,3]
   y <- [4,5,6]
   return (x + y)

we can instead, in applicative style:
(+) <$> [1,2,3] <*> [4,5,6]
-}

{-
ghci
:load Ex2Spec
:reload Ex2Spec
-}
spec :: Spec
spec = do
  describe "Applicative" $ do
    it "todo" $ do
      fmap (,) (ZipList [1, 2]) <*> (ZipList [3, 4]) `shouldBe` ZipList [(1, 3), (2, 4)]    