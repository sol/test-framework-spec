{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module Test.Spec (
-- * Building specifications
-- |
-- The intended behavior of some function is described with `describe` and
-- `it`.  The resulting test suite can be executed with `run`.

-- $example
  run
, describe
, it

-- * Syntactic sugar for HUnit
, shouldBe

-- * Interoperability with test-framework
-- |
-- A specification is basically a newtype-wrapped list of test-framework tests.
-- Use `runSpec` to unwrap it.
, SpecM
, Spec
, runSpec
, add
, pending
, todo
) where

import           Control.Monad.Trans.Writer   (Writer, runWriter)
import qualified Control.Monad.Trans.Writer as Writer
import           Test.Framework (Test, testGroup, defaultMain)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit ((@?=), Assertion, assertFailure)

-- $example
--
-- Here is an example that describes some aspects of @Prelude.reverse@:
--
-- > module Spec where
-- >
-- > import Test.Spec
-- > import Test.Framework.Providers.QuickCheck2
-- >
-- > main :: IO ()
-- > main = run $ do
-- >
-- >   describe "reverse" $ do
-- >     it "reverses a list" $ do
-- >       reverse ([1, 2, 3] :: [Int]) `shouldBe` [3, 2, 1]
-- >
-- >     it "gives the original list, if applied twice" testProperty $
-- >       \xs -> (reverse . reverse) (xs :: [Int]) == xs

newtype SpecM a = SpecM { runSpecM :: Writer [Test] a }
  deriving Monad

type Spec = SpecM ()

-- | Add a test to a specification
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

class IsTest a b | a -> b where
  it :: String -> a -> b

instance IsTest Assertion Spec where
  it label = add . testCase label

instance IsTest (String -> a -> Test) (a -> Spec) where
  it label f = add . f label

-- |
-- @actual \`shouldBe\` expected@ asserts that @actual@ is equal to @expected@
-- (this is just an alias for `@?=`).
shouldBe :: (Show a, Eq a) => a -> a -> Assertion
actual `shouldBe` expected = actual @?= expected


instance IsTest Pending Spec where
  it label (Pending reason) = add $ testCase label $ assertFailure ("pending: " ++ reason)

newtype Pending = Pending String

pending :: String -> Pending
pending = Pending

todo :: Pending
todo = pending "TODO"
