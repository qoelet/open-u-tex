{-# LANGUAGE OverloadedStrings #-}
module TexSpec where

import           Test.Hspec

import           TeX

spec :: Spec
spec =
  describe "mkTeX" $
    it "creates a TeX template for assignments" $ do
      expected <- readFile "test/golden/Foo.tex"
      let student = Student "Foo" "Bar" "foo@bar.com"
          given = mkTeX student "Qux" "Quux" 6
      given `shouldBe` expected
