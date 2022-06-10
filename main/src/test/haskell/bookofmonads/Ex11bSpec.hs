-- {-# LANGUAGE NoImplicitPrelude #-}

module Ex11bSpec (spec) where

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

type State s a = s -> (a, s)

next :: State s a -> (a -> State s b) -> State s b
f `next` g = \i -> let (r, i') = f i in g r i'

pure' :: a -> State s a
pure' x = \i -> (x, i)

relabel :: Tree a -> State Int (Tree (Int, a))
relabel (Leaf x) = \i -> (Leaf (i, x), i + 1)
relabel (Node l r) = relabel l `next` \l' ->
                     relabel r `next` \r' ->
                     pure' (Node l' r')

{-
ghci
:load Ex11bSpec
:reload Ex11bSpec
-}
spec :: Spec
spec = do
  describe "Tree" $ do
    it "relabel leaves" $ do
      let tree = Node
                  (Leaf "a")
                  (Node
                    (Leaf "b")
                    (Node
                      (Leaf "c")
                      (Leaf "d")
                    )
                  ) in
        (relabel tree 1) `shouldBe` (Node
                                      (Leaf (1, "a"))
                                      (Node
                                        (Leaf (2, "b"))
                                        (Node
                                          (Leaf (3, "c"))
                                          (Leaf (4, "d"))
                                        )
                                      ), 5)
