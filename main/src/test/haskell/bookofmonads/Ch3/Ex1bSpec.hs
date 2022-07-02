-- {-# LANGUAGE NoImplicitPrelude #-}

module Ch3.Ex1bSpec (spec) where

import Prelude hiding (map, return, (>>=))
import Test.Hspec

class MonadAsContext m where
  return :: a -> m a
  
  (>>=)  :: m a -> (a -> m b) -> m b

ap :: MonadAsContext m => m (b -> c) -> m b -> m c
ap mbc mb =
  mb >>= \b ->
    mbc >>= \bc ->
      return $ bc b
      
{-
Could also code the above with "do" notation, though it would require the Haskell Monad (and not our custom version):

ap :: Monad m => m (b -> c) -> m b -> m c
ap mbc mb = do
  b  <- mb
  bc <- mbc
  return $ bc b
-}            

{-
ghci
:load Ex1bSpec
:reload Ex1bSpec
-}
spec :: Spec
spec = do
  describe "Monad" $ do
    it "has ap to handle mapping with function(s) already within a context" $ do
      1 `shouldBe` 1    