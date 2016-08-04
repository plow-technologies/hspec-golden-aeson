module Test.Aeson.GenericSpecs (
  --roundtripSpecs,
  --goldenSpecs,

  --shouldBeIdentity,

  -- * re-exports
  module Test.Aeson.Internal.GoldenSpecs
, module Test.Aeson.Internal.RoundtripSpecs
, Proxy(..)
) where

import           Data.Proxy

import           Test.Aeson.Internal.GoldenSpecs
import           Test.Aeson.Internal.RoundtripSpecs
