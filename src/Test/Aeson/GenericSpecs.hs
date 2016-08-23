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
, roundtripAndGoldenADTSpecsWithSettings

  -- * Util
, shouldBeIdentity
, GoldenDirectoryOption(..)
, Settings(..)
, defaultSettings

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
  => Proxy a -> Spec
roundtripAndGoldenSpecs proxy =
  roundtripAndGoldenSpecsWithSettings defaultSettings proxy

roundtripAndGoldenSpecsWithSettings :: forall a.
  (Arbitrary a, Eq a, Show a, ToJSON a, FromJSON a, Typeable a)
  => Settings -> Proxy a -> Spec
roundtripAndGoldenSpecsWithSettings settings proxy = do
  roundtripSpecs proxy
  goldenSpecs settings proxy

-- | run roundtrip and golden tests for all constructors of a type.
-- sampleSize is used only when creating the golden files. When they are
-- compared, the sampleSize is derived from the file.
roundtripAndGoldenADTSpecs :: forall a.
  (Arbitrary a, ToADTArbitrary a, Eq a, Show a, ToJSON a, FromJSON a)
  => Proxy a -> Spec
roundtripAndGoldenADTSpecs proxy =
  roundtripAndGoldenADTSpecsWithSettings defaultSettings proxy

roundtripAndGoldenADTSpecsWithSettings :: forall a.
  (Arbitrary a, ToADTArbitrary a, Eq a, Show a, ToJSON a, FromJSON a)
  => Settings -> Proxy a -> Spec
roundtripAndGoldenADTSpecsWithSettings settings proxy = do
  roundtripADTSpecs proxy
  goldenADTSpecs settings proxy
