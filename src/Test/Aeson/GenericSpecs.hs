module Test.Aeson.GenericSpecs (
  -- for Arbitrary testing
  goldenSpecs
, roundtripSpecs

  -- for ToADTArbitrary testing
, goldenADTSpecs
, roundtripADTSpecs

, shouldBeIdentity

  -- * re-exports
, Proxy(..)
) where

import           Data.Proxy

import           Test.Aeson.Internal.ADT.GoldenSpecs    (goldenADTSpecs)
import           Test.Aeson.Internal.ADT.RoundtripSpecs (roundtripADTSpecs)

import           Test.Aeson.Internal.GoldenSpecs        (goldenSpecs)
import           Test.Aeson.Internal.RoundtripSpecs     (roundtripSpecs)

import           Test.Aeson.Internal.Utils
