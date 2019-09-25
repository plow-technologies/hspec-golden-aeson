{-|
Module      : Test.Aeson
Description : Export all necessary functions
Copyright   : (c) Plow Technologies, 2016-2019
License     : BSD3
Maintainer  : mchaver@gmail.com

-}

{-# LANGUAGE ScopedTypeVariables #-}

module Test.Aeson
  ( roundtripAndGoldenSpecs
  , roundtripAndGoldenSpecsWithSettings
  ) where

-- aeson
import           Data.Aeson    (FromJSON, ToJSON)

-- type classes from base
import           Data.Proxy    (Proxy)
import           Data.Data     (Data)
import           Data.Typeable (Typeable)

-- testing
import           Test.Aeson.Internal.GoldenSpecs        (goldenSpecs)
import           Test.Aeson.Internal.RoundtripSpecs     (roundtripSpecs)
import           Test.Aeson.Internal.Utils              (Settings, defaultSettings)
import           Test.Hspec                             (Spec)
import           Test.QuickCheck                        (Arbitrary)

roundtripAndGoldenSpecs
  :: forall a. (Arbitrary a, ToJSON a, FromJSON a, Data a, Typeable a)
  => Proxy a
  -> Spec
roundtripAndGoldenSpecs proxy =
  roundtripAndGoldenSpecsWithSettings defaultSettings proxy

roundtripAndGoldenSpecsWithSettings
  :: forall a. (Arbitrary a, ToJSON a, FromJSON a, Data a, Typeable a)
  => Settings
  -> Proxy a
  -> Spec
roundtripAndGoldenSpecsWithSettings settings proxy = do
  roundtripSpecs proxy
  goldenSpecs settings proxy
