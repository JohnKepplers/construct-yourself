{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Construction (Context (..), Substitutable (..),
                               Substitution (..), Type (..), compose)
import qualified Data.Map     as M
import           Data.Set
import           Test.Hspec


main :: IO ()
main = hspec $ do
    describe "Substitute monoid test" testSubMonoid
    describe "Context monoid test" testContMonoid
    
singleSub :: Substitution
singleSub = Substitution (M.fromList [("x1", TVar "x2")])

doubleSub :: Substitution
doubleSub = Substitution (M.fromList [("x2", TVar "x3"), ("x3", TVar "x4")])

tripleSub :: Substitution
tripleSub = Substitution (M.fromList [("x1", TVar "x2"), ("x2", TVar "x3"), ("x3", TVar "x4")])

testSubMonoid :: SpecWith ()
testSubMonoid = do
    it "#1" $ (mempty :: Substitution) `shouldBe` Substitution M.empty
    it "#2" $ (mempty `mappend` singleSub) `shouldBe` singleSub
    it "#3" $ (singleSub `mappend` singleSub) `shouldBe` singleSub
    it "#4" $ (singleSub `mappend` doubleSub) `shouldBe` tripleSub
    it "#5" $ (doubleSub `mappend` tripleSub) `shouldBe` tripleSub

testContMonoid :: SpecWith ()
testContMonoid = do
    it "#1" $ (mempty :: Context) `shouldBe` Context M.empty
    it "#2" $ (mempty `mappend` singleSub) `shouldBe` singleSub
    it "#3" $ (singleSub `mappend` singleSub) `shouldBe` singleSub
    it "#4" $ (singleSub `mappend` doubleSub) `shouldBe` tripleSub
    it "#5" $ (doubleSub `mappend` tripleSub) `shouldBe` tripleSub
