-- {-# LANGUAGE NoImplicitPrelude #-}

module Ch1.Ex1Spec (spec) where

import Test.Hspec

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show)

numberOfLeaves :: Tree a -> Integer
numberOfLeaves (Leaf _)   = 1
numberOfLeaves (Node l r) = numberOfLeaves l + numberOfLeaves r

{-
Iterative steps to implementing a function "relabel":

relabel :: Tree a -> Tree (Int, a)
relabel (Leaf x) = Leaf (???, x)

The missing information that we mark with ??? is the index to be returned.
But every Leaf should be relabeled with a different index!
A straightforward solution is to get this information from the outside, as an argument:

relabel :: Tree a -> Int -> Tree (Int, a)
relabel (Leaf x) i = Leaf (i, x)
relabel (Node l r) i = Node (relabel l i) (relabel r ???)

Here we cannot just replace ??? with "i + 1" because what we pass in depends on the counting done by relabelling the left.

What we can do is return that information in addition to the relabeled subtree (giving this ugly implemenation):
-}
relabelIncludingPlumbingToCarryState :: Tree a -> Int -> (Tree (Int, a), Int)
relabelIncludingPlumbingToCarryState (Leaf x) i =
  (Leaf (i, x), i + 1)
relabelIncludingPlumbingToCarryState (Node l r) i =
  let (l', i1) = relabelIncludingPlumbingToCarryState l i
      (r', i2) = relabelIncludingPlumbingToCarryState r i1
  in (Node l' r', i2)

{-
But, we don't like the above because the function handles both logic (recursive relabelling) and the plumbing (carry the state).
-}

type WithCounter a = Int -> (a, Int)

next :: WithCounter a -> (a -> WithCounter b) -> WithCounter b
f `next` g = \i -> let (r, i') = f i in g r i'

pure' :: a -> WithCounter a
pure' x = \i -> (x, i)

relabel :: Tree a -> WithCounter (Tree (a, Int))
relabel (Leaf x) = \i -> (Leaf (x, i), i + 1)
relabel (Node l r) = relabel l `next` \l' ->
                     relabel r `next` \r' ->
                     pure' (Node l' r')

{-
ghci
:load Ex1Spec
:reload Ex1Spec
-}
spec :: Spec
spec = do
  describe "Tree" $ do
    it "count leaves (with let)" $ do
      let tree = (Leaf "a") in
        (numberOfLeaves tree) `shouldBe` 1

    it "count leaves (with where)" $ do
      (numberOfLeaves tree) `shouldBe` 4 where
        tree =
          Node
            (Leaf "a")
            (Node
              (Leaf "b")
              (Node
                (Leaf "c")
                (Leaf "d")
              )
            )