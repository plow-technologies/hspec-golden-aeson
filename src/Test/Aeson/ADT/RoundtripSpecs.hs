{-|
Module      : Test.Aeson.ADT.RoundtripSpecs
Description : Roundtrip tests with arbitraryAdt
Copyright   : (c) Plow Technologies, 2016-2019
License     : BSD3
Maintainer  : mchaver@gmail.com

Internal module, use at your own risk.
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Test.Aeson.ADT.RoundtripSpecs where

-- base
import           Control.Arrow ((>>>))
import           Control.Monad (forM_)

-- aeson
import qualified Data.Aeson as Aeson
import           Data.Data     (Data)
import           Data.Proxy    (Proxy(Proxy))

-- testing
import           Test.Aeson.Internal.Utils     (addBrackets, aesonDecodeIO)
import           Test.Hspec                    (Spec, describe, it, runIO, shouldReturn)
import           Test.QuickCheck               (Arbitrary, generate)
import           Test.QuickCheck.Arbitrary.ADT (arbitraryAdt, adtCAPs, adtTypeName, capArbitrary)


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
roundtripADTSpecs
  :: forall a. (Arbitrary a, Data a, Eq a, Show a, Aeson.ToJSON a, Aeson.FromJSON a)
  => Proxy a
  -> Spec
roundtripADTSpecs proxy = genericAesonRoundtripADTWithNote proxy Nothing

-- | Same as 'roundtripADTSpecs' but has the option of passing a note to the
-- 'describe' function.
genericAesonRoundtripADTWithNote
  :: forall a. (Eq a, Show a, Arbitrary a, Data a, Aeson.ToJSON a, Aeson.FromJSON a)
  => Proxy a
  -> Maybe String
  -> Spec
genericAesonRoundtripADTWithNote _ mNote = do
  adt <- runIO $ generate (arbitraryAdt (Proxy :: Proxy a))
  describe ("JSON encoding of " ++ addBrackets (adtTypeName adt) ++ note) $
    it "allows to encode values with aeson and read them back" $
      forM_ (adtCAPs adt) $ \cap ->
        (Aeson.encode >>> aesonDecodeIO) (capArbitrary cap) `shouldReturn` capArbitrary cap
  where
    note = maybe "" (" " ++) mNote
