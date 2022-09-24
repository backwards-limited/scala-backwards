-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- Functor composition
module Ch10.Ex1Spec (spec) where

-- import Control.Monad.Reader (Reader, runReader)
import Test.Hspec

type Name = String

data Expr = Literal Integer | Var Name | Op Op Expr Expr

data Op   = Add | Subtract | Multiply | Divide

type Assignment = [(Name, Integer)]

eval :: Expr -> Assignment -> Maybe Integer
eval (Literal n) _ = return n
eval (Var v) a     = lookup v a -- returns Nothing if not present
eval (Op o x y) a  =
  do
    u <- eval x a
    v <- eval y a
    case o of
      Add      -> return (u + v)
      Subtract -> return (u - v)
      Multiply -> return (u * v)
      Divide   -> if v == 0 then Nothing else return (u `div` v)

{-
In this code, we make use of the Maybe monad to account for the possibility of failure without having to check whether the result of every step is Just or Nothing.
Still, there is some repetition in this code, because we have to thread the assignment manually through every subcall.

It would be nice if we could use the power of the Reader monad to do this for us:

eval :: Expr -> Reader Assignment (Maybe Integer)

Unfortunately, this means that the monad in the do block is Reader, not Maybe,
so we go back to manually checking whether the result of every operation fails or not:

eval (Op o x y) = do
  u <- eval x
  v <- eval y
  case (u, v) of
    (Nothing, _) -> return Nothing
    (_, Nothing) -> return Nothing
    (Just u', Just v') ->
      case o of
        Add      -> return (Just (u' + v'))
        Subtract -> return (Just (u' - v'))
        Multiply -> return (Just (u' * v'))
        Divide   -> if v' == 0 then return Nothing else return (Just (u' `div` v'))
-}

{-
Our end goal should be to write code that neither checks for failure conditions nor requires manual passing of the assignment:

data Evaluator a = Evaluator (Reader Assignment (Maybe a))

instance Monad Evaluator where
  return x = Evaluator $ Reader $ \_ -> Just x
  (Evaluator x) >>= f = Evaluator $ Reader $ \a ->
    case runReader x a of
      Nothing -> Nothing
      Just y -> let Evaluator e = f y
                  in runReader e a

eval :: Expr -> Evaluator Integer
eval (Literal n) = return n
eval (Var v)     = do
  a <- Evaluator $ fmap Just $ ask
  case lookup v a of
    Nothing -> evalFail
    Just v' -> return v' eval(Opoxy) = do
      u <- eval x
      v <- eval y
      case o of
        Add -> return (u +v)
        Subtract -> return (u - v)
        Multiply -> return (u * v)
        Divide -> if v== 0 then evalFail else return (u `div` v)

evalFail :: Evaluator a
evalFail = Evaluator $ Reader $ \_ -> Nothing
-}

{-
ghci
:load Ex1Spec
:reload Ex1Spec

https://hspec.github.io/options.html
stack test --ta '-m "division by zero"'
-}
spec :: Spec
spec = do
  describe "Exception" $ do
    it "handles division by zero" $ do
      1 `shouldBe` 1