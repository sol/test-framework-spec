module FancySpec where

import Test.Spec
import Test.Framework.Providers.QuickCheck2

main :: IO ()
main = run $ do

  describe "reverse" $ do
    it "reverses a list" $ do
      reverse ([1, 2, 3] :: [Int]) `shouldBe` [3, 2, 1]

    it "gives the original list, if applied twice" testProperty $
      \xs -> (reverse . reverse) (xs :: [Int]) == xs

  describe "pending" $ do
    it "creates a skipped test" $ pending "not yet implemented"

    it "works inside a do block" $ do
      pending "not yet implemented"

  describe "todo" $ do
    it "is a pending test with a default message" todo
