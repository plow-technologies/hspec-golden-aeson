{-# LANGUAGE ScopedTypeVariables #-}

-- | Internal module, use at your own risk.
module Test.Aeson.Internal.RoundtripSpecs where

import           Control.Arrow
import           Control.Exception
import qualified Data.Aeson as Aeson
import           Data.Aeson as Aeson hiding (encode)
import           Data.ByteString.Lazy (ByteString)
import           Data.Typeable

import           Test.Aeson.Internal.Utils
import           Test.Hspec
import           Test.QuickCheck

-- | Allows to obtain a roundtrip test to check whether values of the given type
-- can be successfully converted to JSON and back.
--
-- 'roundtripSpecs' will
--
-- - create random values (using 'Arbitrary'),
-- - convert them into JSON (using 'ToJSON'),
-- - read them back into Haskell (using 'FromJSON') and
-- - make sure that the result is the same as the value it started with
--   (using 'Eq').
roundtripSpecs :: forall a .
  (Typeable a, Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a) =>
  Proxy a -> Spec
roundtripSpecs proxy = genericAesonRoundtripWithNote proxy Nothing

genericAesonRoundtripWithNote :: forall a .
  (Typeable a, Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a) =>
  Proxy a -> Maybe String -> Spec
genericAesonRoundtripWithNote proxy mNote = do
  let note = maybe "" (" " ++) mNote
  describe ("JSON encoding of " ++ addBrackets (show (typeRep proxy)) ++ note) $
    it "allows to encode values with aeson and read them back" $ shouldBeIdentity proxy $
      Aeson.encode >>> aesonDecodeIO
