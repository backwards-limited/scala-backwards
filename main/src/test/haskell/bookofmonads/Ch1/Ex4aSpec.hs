-- {-# LANGUAGE NoImplicitPrelude #-}

module Ch1.Ex4aSpec (spec) where

import Prelude hiding (concatMap, map)
import Test.Hspec

-- Our own simple Lens (to work with ADTs)
data Lens s a =
  Lens {
    get :: s -> a,
    modify :: (a -> a) -> s -> s
  }

isLetter :: Char -> Bool
isLetter c = (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')) 

type Name = String
type Age = Int

data Person = Person { name :: Name, age :: Age } deriving (Show, Eq)

ageL :: Lens Person Age
ageL = Lens {
  get = age,
  modify = \f person -> person { age = f(age person) }
}

map :: (a -> b) -> [a] -> [b]
map f []  = []
map f (x : xs) = f x : map f xs

concatMap :: [a] -> (a -> [b]) -> [b]
concatMap xs f = concat (map f xs)

class MonadAsContext m where
  return :: a -> m a
  
  (>>=)  :: m a -> (a -> m b) -> m b
  
class MonadAsBox m where
  return_ :: a -> m a -- return already defined above, so a wee hack here
  
  fmap   :: (a -> b) -> m a -> m b
  
  join   :: m (m a) -> m a 
  
-- We can define an instance of Monad for Maybe reusing "andThen" which essentially becomes our "bind"

andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen v g =
  case v of
    Nothing -> Nothing
    Just v -> g v
    
{-
instance Monad Maybe where
  return = Just
  
  (>>=)  = andThen
-}

{-
ghci
:load Ex4aSpec
:reload Ex4aSpec
-}
spec :: Spec
spec = do
  describe "List" $ do
    it "concatMap" $ do
      concatMap [1, 10] (\x -> [x + 1, x + 2]) `shouldBe` [2, 3, 11, 12]