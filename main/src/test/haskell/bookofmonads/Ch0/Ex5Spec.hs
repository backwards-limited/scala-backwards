-- {-# LANGUAGE NoImplicitPrelude #-}

module Ch0.Ex5Spec (spec) where

import Test.Hspec

class Container c where
  empty  :: c a
  insert :: a -> c a -> c a

instance Container [] where
  empty = []
  insert = (:) -- insert x xs = x : xs

{-
The above instance inserts new elements at the beginning of the list.
But what if we wanted an implementation that inserted at the end i.e. append, thereby simulating a queue?
We could instead have the following:

instance Container [] where
  empty = []
  insert x xs = xs ++ [x]

So, the following would fail compilation because the compiler would see 2 possible instances of Container for []:

insertTwice :: a -> [a] -> [x]
insertTwice x xs = insert x (insert x xs)
-}

-- To get around the above issue, we can use "newtype" which is simply a memory performant version of "data":
newtype Queue a = Queue [a] deriving Show
{-
Equivalent to (though a tad more awkward):
data Queue a = Queue { unQueue :: [a] }
-}

instance Container Queue where
  empty = Queue []
  insert x (Queue xs) = Queue (xs ++ [x])

instance Eq (Queue String) where
  (Queue s1) == (Queue s2) = s1 == s2

{-
ghci
:load Ex5Spec
:reload Ex5Spec
-}
spec :: Spec
spec = do
  describe "Container" $ do
    it "empty list" $ do
      print $ show (empty :: [Integer])
      (empty :: [Integer]) `shouldBe` []

    it "insert into list" $ do
      print $ show (insert 1 [2])
      insert 1 [2] `shouldBe` [1, 2]

    it "insert into queue" $ do
      print $ show (insert "1" (Queue ["2"]))
      insert "1" (Queue ["2"]) `shouldBe` (Queue ["2", "1"]) -- Note the other way around i.e. append to a queue