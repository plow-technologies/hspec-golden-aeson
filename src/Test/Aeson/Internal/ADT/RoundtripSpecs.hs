{-# LANGUAGE ScopedTypeVariables #-}

-- | Internal module, use at your own risk.
module Test.Aeson.Internal.ADT.RoundtripSpecs where

import           Control.Arrow
import           Control.Exception

import qualified Data.Aeson as Aeson
import           Data.Aeson as Aeson hiding (encode)
import           Data.ByteString.Lazy (ByteString)
import           Data.Typeable

import           Test.Aeson.Internal.Utils
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.ADT

import Control.Monad

-- | A roundtrip test to check whether values of the given type
-- can be successfully converted to JSON and back to a Haskell value.
--
-- 'roundtripSpecs' will
--
-- - create random values using 'Arbitrary',
-- - convert them into JSON using 'ToJSON',
-- - read them back into Haskell using 'FromJSON' and
-- - make sure that the result is the same as the value it started with
--   using 'Eq'.
roundtripADTSpecs :: forall a.
  (ToADTArbitrary a, Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a)
  => Proxy a
  -> Spec
roundtripADTSpecs proxy = genericAesonRoundtripADTWithNote proxy Nothing

genericAesonRoundtripADTWithNote :: forall a.
  (ToADTArbitrary a, Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a)
  => Proxy a
  -> Maybe String
  -> Spec
genericAesonRoundtripADTWithNote proxy mNote = do
  adt <- runIO $ generate (toADTArbitrary (Proxy :: Proxy a))
  describe ("JSON encoding of " ++ addBrackets (_adtTypeName adt) ++ note) $
    it "allows to encode values with aeson and read them back" $
      forM_ (_adtCAPs adt) $ \cap ->
        (Aeson.encode >>> aesonDecodeIO) (_capArbitrary cap) `shouldReturn` _capArbitrary cap
  where
    note = maybe "" (" " ++) mNote
