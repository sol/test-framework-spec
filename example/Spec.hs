module Spec where

import Test.Spec

main :: IO ()
main = run $ do

  describe "reverse" $ do
    it "reverses a list" $ do
      reverse ([1, 2, 3] :: [Int]) `shouldBe` [3, 2, 1]

    prop "gives the original list, if applied twice" $
      \xs -> (reverse . reverse) (xs :: [Int]) == xs
