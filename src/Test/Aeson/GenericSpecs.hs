module Test.Aeson.GenericSpecs (
  --roundtripSpecs,
  --goldenSpecs,

  --shouldBeIdentity,

  -- * re-exports
  module Test.Aeson.Internal.GoldenSpecs
, Proxy(..)
) where

import           Data.Proxy

import           Test.Aeson.Internal.GoldenSpecs
import           Test.Aeson.Internal.RoundtripSpecs
