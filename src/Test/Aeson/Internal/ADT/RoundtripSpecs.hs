{-|
Module      : Test.Aeson.Internal.ADT.RoundtripSpecs
Description : Roundtrip tests for ToADTArbitrary
Copyright   : (c) Plow Technologies, 2016
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : Beta

Internal module, use at your own risk.
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Test.Aeson.Internal.ADT.RoundtripSpecs where

import           Control.Arrow

import qualified Data.Aeson as Aeson
import           Data.Aeson as Aeson hiding (encode)
import           Data.Typeable

import           Test.Aeson.Internal.Utils
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.ADT

import Control.Monad

-- | A roundtrip test to check whether values of all of constructors of the
-- given type can be successfully converted to JSON and back to a Haskell value.
--
-- 'roundtripADTSpecs' will
--
-- - create random values for each constructor using 'ToADTArbitrary',
-- - convert them into JSON using 'ToJSON',
-- - read them back into Haskell using 'FromJSON' and
-- - make sure that the result is the same as the value it started with
--   using 'Eq'.
roundtripADTSpecs :: forall a.
  (Arbitrary a, ToADTArbitrary a, Eq a, Show a, ToJSON a, FromJSON a)
  => Proxy a
  -> Spec
roundtripADTSpecs proxy = genericAesonRoundtripADTWithNote proxy Nothing

-- | Same as 'roundtripADTSpecs' but has the option of passing a note to the
-- 'describe' function.
genericAesonRoundtripADTWithNote :: forall a.
  (ToADTArbitrary a, Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a)
  => Proxy a
  -> Maybe String
  -> Spec
genericAesonRoundtripADTWithNote _ mNote = do
  adt <- runIO $ generate (toADTArbitrary (Proxy :: Proxy a))
  describe ("JSON encoding of " ++ addBrackets (adtTypeName adt) ++ note) $
    it "allows to encode values with aeson and read them back" $
      forM_ (adtCAPs adt) $ \cap ->
        (Aeson.encode >>> aesonDecodeIO) (capArbitrary cap) `shouldReturn` capArbitrary cap
  where
    note = maybe "" (" " ++) mNote
