module Spec where

import Test.Spec
import Test.Framework.Providers.QuickCheck2

main :: IO ()
main = run $ do

  describe "reverse" $ do
    it "reverses a list" $ do
      reverse ([1, 2, 3] :: [Int]) `shouldBe` [3, 2, 1]

    it "gives the original list, if applied twice" testProperty $
      \xs -> (reverse . reverse) (xs :: [Int]) == xs
