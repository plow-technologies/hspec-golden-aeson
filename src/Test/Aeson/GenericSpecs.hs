{-|
Module      : Test.Aeson.GenericSpecs
Description : Export all necessary functions
Copyright   : (c) Plow Technologies, 2016
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : Beta

This package provides tools for testing Aeson serialization.

- Test that 'ToJSON' and 'FromJSON' instances are isomorphic.
- Alert you when unexpected changes in Aeson serialization occur.
- Record JSON formatting of Haskell types.

-}

{-# LANGUAGE ScopedTypeVariables #-}

module Test.Aeson.GenericSpecs
  (
    -- * Arbitrary testing
    goldenSpecs
  , roundtripSpecs
  , roundtripAndGoldenSpecs
  , roundtripFromFile
  , roundtripFromFileWithSettings

  -- * ToADTArbitrary testing
  , goldenADTSpecs
  , roundtripADTSpecs
  , roundtripAndGoldenSpecsWithSettings
  , roundtripAndGoldenADTSpecs
  , roundtripAndGoldenADTSpecsWithSettings
  , roundtripADTFromFile
  , roundtripADTFromFileWithSettings

  -- * Make Files
  , mkGoldenFileForType
  
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

import           Test.Aeson.Internal.ADT.GoldenSpecs    (goldenADTSpecs, mkGoldenFileForType)
import           Test.Aeson.Internal.ADT.RoundtripSpecs (roundtripADTSpecs)
import           Test.Aeson.Internal.GoldenSpecs        (goldenSpecs)
import           Test.Aeson.Internal.RoundtripSpecs     (roundtripSpecs)
import qualified Test.Aeson.Internal.RoundtripFromFile
import qualified Test.Aeson.Internal.ADT.RoundtripFromFile
import           Test.Aeson.Internal.Utils
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.ADT

-- | run roundtrip and golden test for a type.
-- sampleSize is used only when creating the golden file. When it is
-- compared, the sampleSize is derived from the file.
roundtripAndGoldenSpecs :: forall a.
  (Arbitrary a, ToJSON a, FromJSON a, Typeable a)
  => Proxy a -> Spec
roundtripAndGoldenSpecs proxy =
  roundtripAndGoldenSpecsWithSettings defaultSettings proxy

-- | 'roundtripAndGoldenSpecs' with custom settings.
roundtripAndGoldenSpecsWithSettings :: forall a.
  (Arbitrary a, ToJSON a, FromJSON a, Typeable a)
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

-- | 'roundtripAndGoldenADTSpecs' with custom settings.
roundtripAndGoldenADTSpecsWithSettings :: forall a.
  (Arbitrary a, ToADTArbitrary a, Eq a, Show a, ToJSON a, FromJSON a)
  => Settings -> Proxy a -> Spec
roundtripAndGoldenADTSpecsWithSettings settings proxy = do
  roundtripADTSpecs proxy
  goldenADTSpecs settings proxy

roundtripFromFile :: forall a. 
  (Arbitrary a, Typeable a, Eq a, Show a, ToJSON a, FromJSON a)
  => Proxy a -> Spec
roundtripFromFile = roundtripFromFileWithSettings defaultSettings

roundtripFromFileWithSettings :: forall a. 
  (Arbitrary a, Typeable a, Eq a, Show a, ToJSON a, FromJSON a)
  => Settings -> Proxy a -> Spec
roundtripFromFileWithSettings = Test.Aeson.Internal.RoundtripFromFile.roundtripFromFile

roundtripADTFromFile :: forall a. 
  (ToADTArbitrary a, Typeable a, Eq a, Show a, ToJSON a, FromJSON a)
  => Proxy a -> Spec
roundtripADTFromFile = roundtripADTFromFileWithSettings defaultSettings

roundtripADTFromFileWithSettings :: forall a. 
  (ToADTArbitrary a, Typeable a, Eq a, Show a, ToJSON a, FromJSON a)
  => Settings -> Proxy a -> Spec
roundtripADTFromFileWithSettings = Test.Aeson.Internal.ADT.RoundtripFromFile.roundtripADTFromFile