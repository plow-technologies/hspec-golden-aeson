{-# LANGUAGE ScopedTypeVariables #-}

module Test.Aeson.GenericSpecs (
  -- for Arbitrary testing
  goldenSpecs
, roundtripSpecs
, roundtripAndGoldenSpecs

  -- for ToADTArbitrary testing
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

roundtripAndGoldenSpecs :: forall a.
  (Arbitrary a, Eq a, Show a, ToJSON a, FromJSON a, Typeable a)
  => Proxy a
  -> Spec
roundtripAndGoldenSpecs Proxy = do
  roundtripSpecs (Proxy :: Proxy a)
  goldenSpecs (Proxy :: Proxy a)

roundtripAndGoldenADTSpecs :: forall a.
  (Arbitrary a, ToADTArbitrary a, Eq a, Show a, ToJSON a, FromJSON a)
  => Proxy a
  -> Spec
roundtripAndGoldenADTSpecs Proxy = do
  roundtripADTSpecs (Proxy :: Proxy a)
  goldenADTSpecs (Proxy :: Proxy a)
