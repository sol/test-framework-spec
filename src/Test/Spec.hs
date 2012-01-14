{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Spec (
-- * Building specifications
-- |
-- The intended behavior of some function is described with `describe`, `it`
-- and `prop`.  The resulting test suite can be executed with `run`.

-- $example
  run
, describe
, it
, prop

-- * Syntactic sugar for HUnit
, shouldBe

-- * Interoperability with test-framework
-- |
-- A specification is basically a newtype-wrapped list of test-framework tests.
-- Use `runSpec` to unwrap it.
, SpecM
, Spec
, runSpec
) where

import           Control.Monad.Trans.Writer   (Writer, runWriter)
import qualified Control.Monad.Trans.Writer as Writer
import           Test.Framework (Test, testGroup, defaultMain)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit ((@?=), Assertion)
import           Test.QuickCheck(Testable)

-- $example
--
-- Here is an example that describes some aspects of @Prelude.reverse@:
--
-- > module Spec where
-- >
-- > import Test.Spec
-- >
-- > main :: IO ()
-- > main = run $ do
-- >
-- >   describe "reverse" $ do
-- >     it "reverses a list" $ do
-- >       reverse ([1, 2, 3] :: [Int]) `shouldBe` [3, 2, 1]
-- >
-- >     prop "gives the original list, if applied twice" $
-- >       \xs -> (reverse . reverse) (xs :: [Int]) == xs

newtype SpecM a = SpecM { runSpecM :: Writer [Test] a }
  deriving Monad

type Spec = SpecM ()

add :: Test -> Spec
add = SpecM . Writer.tell . return

-- | Convert a specification to a list of tests.
runSpec :: Spec -> [Test]
runSpec = snd . runWriter . runSpecM

-- | Run a specification.  This is equivalent to @`defaultMain` . `runSpec`@.
run :: Spec -> IO ()
run = defaultMain . runSpec

describe :: String -> Spec -> Spec
describe label = add . testGroup label . runSpec

it :: String -> Assertion -> Spec
it label = add . testCase label

prop :: Testable a => String -> a -> Spec
prop label = add . testProperty label

-- |
-- @actual \`shouldBe\` expected@ asserts that @actual@ is equal to @expected@
-- (this is just an alias for `@?=`).
shouldBe :: (Show a, Eq a) => a -> a -> Assertion
actual `shouldBe` expected = actual @?= expected