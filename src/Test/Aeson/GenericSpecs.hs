{-# LANGUAGE ScopedTypeVariables #-}

module Test.Aeson.GenericSpecs (
  -- * Arbitrary testing
  goldenSpecs
, roundtripSpecs
, roundtripAndGoldenSpecs

  -- * ToADTArbitrary testing
, goldenADTSpecs
, roundtripADTSpecs
, roundtripAndGoldenADTSpecs

, shouldBeIdentity

  -- * re-exports
, Proxy(..)
) where

import           Data.Aeson                             (FromJSON, ToJSON)
import           Data.Proxy
import           Data.Typeable

import           Test.Aeson.Internal.ADT.GoldenSpecs    (goldenADTSpecs)
import           Test.Aeson.Internal.ADT.RoundtripSpecs (roundtripADTSpecs)
import           Test.Aeson.Internal.GoldenSpecs        (goldenSpecs)
import           Test.Aeson.Internal.RoundtripSpecs     (roundtripSpecs)
import           Test.Aeson.Internal.Utils
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.ADT

-- | run roundtrip and golden test for a type.
-- sampleSize is used only when creating the golden file. When it is
-- compared, the sampleSize is derived from the file.
roundtripAndGoldenSpecs :: forall a.
  (Arbitrary a, Eq a, Show a, ToJSON a, FromJSON a, Typeable a)
  => Int
  -> Proxy a
  -> Spec
roundtripAndGoldenSpecs sampleSize proxy = do
  roundtripSpecs proxy
  goldenSpecs sampleSize proxy

-- | run roundtrip and golden tests for all constructors of a type.
-- sampleSize is used only when creating the golden files. When they are
-- compared, the sampleSize is derived from the file.
roundtripAndGoldenADTSpecs :: forall a.
  (Arbitrary a, ToADTArbitrary a, Eq a, Show a, ToJSON a, FromJSON a)
  => Int
  -> Proxy a
  -> Spec
roundtripAndGoldenADTSpecs sampleSize proxy = do
  roundtripADTSpecs proxy
  goldenADTSpecs sampleSize proxy
