{-|
Module      : Test.Aeson.Internal.RoundtripSpecs
Description : Roundtrip tests for Arbitrary
Copyright   : (c) Plow Technologies, 2016
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : Beta

Internal module, use at your own risk.
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Test.Aeson.Internal.RoundtripSpecs where


import           Data.Aeson as Aeson hiding (encode)
import           Data.Typeable

import           Test.Aeson.Internal.Utils
import           Test.Hspec
import           Test.QuickCheck
import           Test.Hspec.QuickCheck
-- | A roundtrip test to check whether values of the given type
-- can be successfully converted to JSON and back to a Haskell value.
--
-- 'roundtripSpecs' will
--
-- - create random values (using 'Arbitrary'),
-- - convert them into JSON (using 'ToJSON'),
-- - read them back into Haskell (using 'FromJSON') and
-- - make sure that the result is the same as the value it started with
--   (using 'Eq').
roundtripSpecs :: forall a .
  (Typeable a, Arbitrary a, ToJSON a, FromJSON a) =>
  Proxy a -> Spec
roundtripSpecs proxy = genericAesonRoundtripWithNote proxy Nothing

-- | Same as 'roundtripSpecs', but optionally add notes to the 'describe'
-- function.
genericAesonRoundtripWithNote :: forall a .
  (Typeable a, Arbitrary a, ToJSON a, FromJSON a) =>
  Proxy a -> Maybe String -> Spec
genericAesonRoundtripWithNote proxy mNote = do
  let typeIdentifier = show (typeRep proxy)
  result <- genericAesonRoundtripWithNotePlain proxy mNote typeIdentifier
  return result

-- | Same as 'genericAesonRoundtripWithNote', but no need for Typeable, Eq, or Show
genericAesonRoundtripWithNotePlain :: forall a .
  (Arbitrary a, ToJSON a, FromJSON a) =>
  Proxy a -> Maybe String -> String -> Spec
genericAesonRoundtripWithNotePlain _ mNote typeIdentifier = do
  let note = maybe "" (" " ++) mNote
      checkAesonEncodingEquality' :: JsonShow a -> Bool
      checkAesonEncodingEquality' = checkAesonEncodingEquality
  
  describe ("JSON encoding of " ++ addBrackets (typeIdentifier) ++ note) $
    prop "allows to encode values with aeson and read them back"  
          (checkAesonEncodingEquality' )


