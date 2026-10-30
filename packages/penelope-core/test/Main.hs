{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Penelope.Core

main :: IO ()
main = hspec $ do
  describe "Penelope.Core" $ do
    it "constructs default entity with correct properties" $ do
      let e = defaultEntity
      entityName e `shouldBe` "penelope"
      entityModule e `shouldBe` "Penelope.Core"
    it "renders entity accurately" $ do
      let e = defaultEntity
      renderEntity e `shouldBe` "penelope:Penelope.Core"
