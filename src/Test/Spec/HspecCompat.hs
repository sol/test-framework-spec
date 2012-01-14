module Test.Spec.HspecCompat (
-- * Compatibility with Hspec
-- |
-- The following functions provide some compatibility with the monadic
-- interface of Hspec.  Just import this module instead of
-- @Test.Hspec.Monadic@, @Test.Hspec.HUnit@ and @Test.Hspec.QuickCheck@.
  hspec
, hspecX
, module Test.Spec
) where

import Test.Spec

hspec :: Spec -> IO ()
hspec = run

hspecX :: Spec -> IO ()
hspecX = run
