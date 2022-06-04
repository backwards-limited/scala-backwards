-- {-# LANGUAGE NoImplicitPrelude #-}

module Ex02Spec (spec) where

import Test.Hspec

{-
class Eq a where
  (==) :: a -> a -> Bool
  
eqList :: Eq a => [a] -> [a] -> Bool
eqList [] [] = True
eqList (x : xs) (y : ys) = x == y && eqList xs ys
eqList _ _ = False
  
instance Eq a => Eq [a] where
  (==) = eqList  
-}

data MyTuple a b = MyTuple a b

instance (Eq a, Eq b) => Eq (MyTuple a b) where
  (MyTuple a b) == (MyTuple a' b') =
    a == a' && b == b'

{-
ghci
:load Ex02
:reload Ex02
-}
spec :: Spec
spec = do
  describe "Eq" $ do
    it "compares tuples" $ do
      print $ show ((MyTuple "1" "2") == (MyTuple "1" "2"))
      ((MyTuple "1" "2") == (MyTuple "1" "2")) `shouldBe` True
      
      print $ show ((MyTuple 1 2) == (MyTuple 1 2))
      ((MyTuple 1 2) == (MyTuple 1 2)) `shouldBe` True
      
      print $ show ((MyTuple 1 2) == (MyTuple 2 1))
      ((MyTuple 1 2) == (MyTuple 2 1)) `shouldBe` False