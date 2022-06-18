-- {-# LANGUAGE NoImplicitPrelude #-}

module Ch1.Ex5Spec (spec) where

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
ghci
:load Ex5Spec
:reload Ex5Spec
-}
spec :: Spec
spec = do
  describe "Option" $ do
    it "validate name" $ do
      validateName "Scooby" `shouldBe` Just "Scooby"