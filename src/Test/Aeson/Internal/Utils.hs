{-|
Module      : Test.Aeson.Internal.Utils
Description : Utility types, functions and values
Copyright   : (c) Plow Technologies, 2016
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : Beta
-}


{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}

module Test.Aeson.Internal.Utils where

import           Control.Exception

import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy

import           Prelude

import           Test.Hspec
import           Test.QuickCheck


data Settings = Settings {
  goldenDirectoryOption       :: GoldenDirectoryOption -- ^ use a custom directory name or use the generic "golden" directory.

, useModuleNameAsSubDirectory :: Bool -- ^ If true, use the module name in the file path, otherwise ignore it.

, sampleSize                  :: Int -- ^ How many instances of each type you want. If you use ADT versions than it will use the sample size for each constructor.
}

-- | A custom directory name or a preselected directory name.
data GoldenDirectoryOption = CustomDirectoryName String | GoldenDirectory

-- | The default settings for general use cases.
defaultSettings :: Settings
defaultSettings = Settings GoldenDirectory False 5

-- | put brackets around a String.
addBrackets :: String -> String
addBrackets s =
  if ' ' `elem` s
    then "(" ++ s ++ ")"
    else s

-- | [hspec](http://hspec.github.io/) style combinator to easily write tests
-- that check the a given operation returns the same value it was given, e.g.
-- roundtrip tests.
shouldBeIdentity :: (Eq a, Show a, Arbitrary a) =>
  Proxy a -> (a -> IO a) -> Property
shouldBeIdentity Proxy function =
  property $ \ (a :: a) -> function a `shouldReturn` a

-- | This function will compare one JSON encoding to a subsequent JSON encoding, thus eliminating the need for an Eq instance
checkAesonEncodingEquality :: forall a . (ToJSON a, FromJSON a) => JsonShow a -> Bool
checkAesonEncodingEquality (JsonShow a) =  
  let byteStrA = encode a
      decodedVal =  (eitherDecode byteStrA) :: Either String a
      eitherByteStrB = encode <$> decodedVal  
  in (Right byteStrA) == eitherByteStrB

-- | run decode in IO, if it returns Left then throw an error.
aesonDecodeIO :: FromJSON a => ByteString -> IO a
aesonDecodeIO bs = case eitherDecode bs of
  Right a -> return a
  Left msg -> throwIO $ ErrorCall
    ("aeson couldn't parse value: " ++ msg)

newtype JsonShow a = JsonShow a 

instance ToJSON a => Show (JsonShow a ) where 
    show (JsonShow v) = show . encode $ v 

instance ToJSON a => ToJSON (JsonShow a) where
    toJSON (JsonShow a) = toJSON a

instance FromJSON a => FromJSON (JsonShow a) where
     parseJSON v = JsonShow <$> (parseJSON v)

instance Arbitrary a => Arbitrary (JsonShow a) where
    arbitrary = JsonShow <$> arbitrary 
