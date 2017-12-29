{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Construction     (Context (..), Name, Substitution (..), Term (..), Type (..), substitute1)
import           Data.Text        hiding (singleton)
import           Tasks
import           Test.Hspec
import qualified Text.Parsec      as TP (parse)
import           Text.Parsec.Text
import           Data.Map
import qualified Data.Set         as S (fromList)
import qualified Data.Map                        as M ((!), empty)

main :: IO ()
main = do

  hspec $ do
    describe "isMonoid test for Context" isMonoidContextTest
    describe "isMonoid test for Substitution" isMonoidSubstitutionTest



context1 = Context (singleton "x" (TVar "x1"))
context2 = Context (singleton "y" (TVar "y1"))
context3 = Context (singleton "z" (TVar "z1"))
context4 = Context ( singleton "x" (TVar "x1") `mappend` singleton "y" (TVar "y1"))

isMonoidContextTest :: SpecWith ()
isMonoidContextTest = do
  it "#1" $ (mempty) `shouldBe` Context M.empty
  it "#2" $ (context1 `mappend` mempty) `shouldBe` context1
  it "#3" $ (mempty `mappend` context1) `shouldBe` context1
  it "#4" $ (context1 `mappend` context2)`mappend` context3 `shouldBe` context1 `mappend` (context2 `mappend` context3)
  it "#5" $ (context1 `mappend` context2) `shouldBe` context4
  
substitution1 = Substitution (singleton "x" (TVar "x1"))
substitution2 = Substitution (singleton "y" (TVar "y1"))
substitution3 = Substitution (singleton "z" (TVar "z1"))
substitution4 = Substitution (singleton "x" (TVar "x1") `mappend` singleton "y" (TVar "y1"))

isMonoidSubstitutionTest :: SpecWith ()
isMonoidSubstitutionTest = do
  it "#1" $ (mempty) `shouldBe` Substitution M.empty
  it "#2" $ (mempty `mappend` substitution1) `shouldBe` substitution1
  it "#3" $ (substitution1 `mappend` mempty) `shouldBe` substitution1
  it "#4" $ (substitution1 `mappend` substitution2)`mappend` substitution3 `shouldBe` substitution1 `mappend` (substitution2 `mappend` substitution3)
  it "#5" $ (substitution1 `mappend` substitution2) `shouldBe` substitution4

