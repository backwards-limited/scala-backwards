-- {-# LANGUAGE NoImplicitPrelude #-}

-- Better notation - including rewrite of previous code to use Monad
module Ch2.Ex1Spec (spec) where

import Prelude hiding (map, return, (>>=))
import Test.Hspec

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show)

newtype State s a = State (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (State f) = f
  
class MonadAsContext m where
  return :: a -> m a
  
  (>>=)  :: m a -> (a -> m b) -> m b
  
instance MonadAsContext (State s) where
  return a =
    State(\s -> (a, s))
    
  (State f) >>= g =
    State (\s -> let (a, s') = f s
                     s''     = g a
                 in runState s'' s')

relabel :: Tree a -> State Int (Tree (Int, a))
relabel (Leaf x) =
  State (\i -> (Leaf (i, x), i + 1))
relabel (Node l r) =
  relabel l >>= \l' ->
    relabel r >>= \r' ->
      return (Node l' r')

-- relabelDo :: Tree a -> State Int (Tree (Int, a))
relabelDo (Leaf x) =
  State (\i -> (Leaf (i, x), i + 1))
relabelDo (Node l r) =
  do  l' <- relabelDo l
      r' <- relabelDo r
      return (Node l' r')

type Name = String
type Age = Int

data Person = Person { name :: Name, age :: Age } deriving (Show, Eq)

isLetter :: Char -> Bool
isLetter c = (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))

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

instance MonadAsContext Maybe where
  return = Just

  (Just a) >>= g =
    g a

  Nothing >>= g =
    Nothing

validatePerson name age =
  validateName name >>= \name ->
    validateAge  age  >>= \age ->
      return (Person name age)

validatePersonDo name age = do
  name' <- validateName name
  age' <- validateAge age
  return (Person name' age')

{-
ghci
:load Ex1Spec
:reload Ex1Spec
-}
spec :: Spec
spec = do
  describe "Tree" $ do
    it "relabel leaves" $ do
      let tree = Node
                  (Leaf "a")
                  (Node
                    (Leaf "b")
                    (Node (Leaf "c")(Leaf "d"))
                  )
        in (runState (relabel tree) 1) `shouldBe`
        (Node
          (Leaf (1, "a"))
          (Node
            (Leaf (2, "b"))
            (Node (Leaf (3, "c")) (Leaf (4, "d")))
          ), 5)

  describe "Option" $ do
    it "validate Persion" $ do
      validatePerson "Scooby" 5 `shouldBe` Just (Person "Scooby" 5)