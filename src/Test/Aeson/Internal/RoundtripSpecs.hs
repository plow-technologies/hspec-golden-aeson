{-# LANGUAGE ScopedTypeVariables #-}

-- | Internal module, use at your own risk.
module Test.Aeson.Internal.RoundtripSpecs where

import           Control.Arrow
import           Control.Exception
import qualified Data.Aeson as Aeson
import           Data.Aeson as Aeson hiding (encode)
import           Data.ByteString.Lazy (ByteString)
import           Data.Typeable
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
roundtripSpecs :: forall a.
  (Typeable a, Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a)
  => Proxy a
  -> Spec
roundtripSpecs proxy = genericAesonRoundtripWithNote proxy Nothing

constructorRoundtripSpecs :: forall a.
  (ToADTArbitrary a, Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a)
  => Proxy a
  -> Spec
constructorRoundtripSpecs proxy = genericAesonConstructorRoundtripWithNote proxy Nothing

genericAesonRoundtripWithNote :: forall a .
  (Typeable a, Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a)
  => Proxy a
  -> Maybe String
  -> Spec
genericAesonRoundtripWithNote proxy mNote = do
  let note = maybe "" (" " ++) mNote
  describe ("JSON encoding of " ++ addBrackets (show (typeRep proxy)) ++ note) $
    it "allows to encode values with aeson and read them back" $
      shouldBeIdentity proxy $
        Aeson.encode >>> aesonDecodeIO

genericAesonConstructorRoundtripWithNote :: forall a.
  (ToADTArbitrary a, Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a)
  => Proxy a
  -> Maybe String
  -> Spec
genericAesonConstructorRoundtripWithNote proxy mNote = do
  adt <- runIO $ generate (toADTArbitrary (Proxy :: Proxy a))
  describe ("JSON encoding of " ++ addBrackets (_adtTypeName adt) ++ note) $
    it "allows to encode values with aeson and read them back" $
      forM_ (_adtCAPs adt) $ \cap ->
        (Aeson.encode >>> aesonDecodeIO) (_capArbitrary cap) `shouldReturn` _capArbitrary cap
        -- shouldBeIdentity proxy $ Aeson.encode >>> aesonDecodeIO

  where
    note = maybe "" (" " ++) mNote


addBrackets :: String -> String
addBrackets s =
  if ' ' `elem` s
    then "(" ++ s ++ ")"
    else s

-- | [hspec](http://hspec.github.io/) style combinator to easily write tests
-- that check that a given operation returns the same value it was given, e.g.
-- roundtrip tests.
shouldBeIdentity :: forall a.
  (Eq a, Show a, Arbitrary a)
  => Proxy a
  -> (a -> IO a)
  -> Property
shouldBeIdentity Proxy function =
  property $ \ (a :: a) -> function a `shouldReturn` a

aesonDecodeIO :: forall a.
  (FromJSON a)
  => ByteString
  -> IO a
aesonDecodeIO bs = case eitherDecode bs of
  Right a -> return a
  Left msg -> throwIO $ ErrorCall
    ("aeson couldn't parse value: " ++ msg)
