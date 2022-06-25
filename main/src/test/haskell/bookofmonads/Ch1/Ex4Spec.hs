-- {-# LANGUAGE NoImplicitPrelude #-}

module Ch1.Ex4Spec (spec) where

import Prelude hiding (map)
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

validateName :: Name -> Maybe Name
validateName name =
  if (filter isLetter name) == name then
    Just name
  else
    Nothing
    
validateAge :: Age -> Maybe Age
validateAge age =
  if (age > 0 && age < 110) then
    Just age
  else
    Nothing      

validatePersonUgly :: Name -> Age -> Maybe Person
validatePersonUgly name age =
  case validateName name of
    Nothing -> Nothing
    Just name -> case validateAge age of
      Nothing -> Nothing
      Just age -> Just (Person name age)

-- To get away from ugly:
andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen v g =
  case v of
    Nothing -> Nothing
    Just v -> g v
    
validatePerson :: Name -> Age -> Maybe Person
validatePerson name age =
  validateName name `andThen` \ name ->
    validateAge age `andThen` \ age ->
      Just (Person name age)
      
{-
We have the following similarity:

next    :: State s a -> (a -> State s b) -> State s b

andThen :: Maybe   a -> (a -> Maybe   b) -> Maybe   b
-}

map :: (a -> b) -> Maybe a -> Maybe b
map f Nothing  = Nothing
map f (Just x) = Just (f x)

singleton :: a -> Maybe a
singleton = Just

flatten :: Maybe (Maybe a) -> Maybe a
flatten (Just (Just x)) = Just x
flatten _               = Nothing

{-
ghci
:load Ex4Spec
:reload Ex4Spec
-}
spec :: Spec
spec = do
  describe "Option" $ do
    it "validate name" $ do
      validateName "Scooby" `shouldBe` Just "Scooby"
      
    it "invalidate name" $ do
      validateName "Scooby1" `shouldBe` Nothing
      
    it "validate age" $ do
      validateAge 5 `shouldBe` Just 5
      
    it "invalidate age" $ do
      validateAge 555 `shouldBe` Nothing 
      
    it "validate Person the ugly way" $ do
      validatePersonUgly "Scooby" 5 `shouldBe` Just (Person "Scooby" 5)
      
    it "invalidate Person the ugly way" $ do
      validatePersonUgly "Scooby" 555 `shouldBe` Nothing
      
    it "validate Person" $ do
      validatePerson "Scooby" 5 `shouldBe` Just (Person "Scooby" 5)
      
    it "map Person" $ do
      map (modify ageL (+ 1)) (Just (Person "Scooby" 5)) `shouldBe` Just (Person "Scooby" 6)