{-|
Module      : Test.Aeson.Internal.Utils
Description : Internal types, functions and values
Copyright   : (c) Plow Technologies, 2016
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : Beta
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}

module Test.Aeson.Internal.Utils where

import           Control.Exception

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy
import           Data.Typeable

import           Prelude

import           Test.Hspec
import           Test.QuickCheck

-- | Option to indicate whether to create a separate comparison file or overwrite the golden file.
-- A separate file allows you to use `diff` to compare.
-- Overwriting allows you to use source control tools for comparison.
data ComparisonFile
  = FaultyFile
  -- ^ Create a new faulty file when tests fail
  | OverwriteGoldenFile
  -- ^ Overwrite the golden file when tests fail

-- | Option indicating whether to fail tests when the random seed does not produce the same values as in the golden file.
-- Default is to output a warning.
data RandomMismatchOption
  = RandomMismatchWarning
  -- ^ Only output a warning when the random seed does not produce the same values
  | RandomMismatchError
  -- ^ Fail the test when the random seed does not produce the same value

data Settings = Settings 
  { goldenDirectoryOption :: GoldenDirectoryOption
  -- ^ use a custom directory name or use the generic "golden" directory.
  , useModuleNameAsSubDirectory :: Bool
  -- ^ If true, use the module name in the file path, otherwise ignore it.
  , sampleSize :: Int
  -- ^ How many instances of each type you want. If you use ADT versions than it will use the sample size for each constructor.
  , comparisonFile :: ComparisonFile
  -- ^ Whether to create a separate comparison file or ovewrite the golden file.
  , randomMismatchOption :: RandomMismatchOption
  -- ^ Whether to output a warning or fail the test when the random seed produces different values than the values in the golden file.
  }

-- | A custom directory name or a preselected directory name.
data GoldenDirectoryOption = CustomDirectoryName String | GoldenDirectory

-- | The default settings for general use cases.
defaultSettings :: Settings
defaultSettings = Settings GoldenDirectory False 5 FaultyFile RandomMismatchWarning

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
shouldBeIdentity Proxy func =
  property $ \ (a :: a) -> func a `shouldReturn` a

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
  Left msg -> throwIO $ AesonDecodeError msg

data AesonDecodeError = AesonDecodeError String
  deriving (Show, Eq)

instance Exception AesonDecodeError

-- | Used to eliminate the need for an Eq instance
newtype JsonShow a = JsonShow a 

instance ToJSON a => Show (JsonShow a) where 
    show (JsonShow v) = show . encode $ v 

instance ToJSON a => ToJSON (JsonShow a) where
    toJSON (JsonShow a) = toJSON a

instance FromJSON a => FromJSON (JsonShow a) where
     parseJSON v = JsonShow <$> (parseJSON v)

instance Arbitrary a => Arbitrary (JsonShow a) where
    arbitrary = JsonShow <$> arbitrary 

--------------------------------------------------
-- Handle creating names
--------------------------------------------------

newtype TopDir =
  TopDir
    { unTopDir :: FilePath
    } deriving (Eq,Read,Show)

newtype ModuleName =
  ModuleName
    { unModuleName :: FilePath
    } deriving (Eq,Read,Show)

newtype TypeName =
  TypeName
    { unTypeName :: FilePath
    } deriving (Eq,Read,Show)

data TypeNameInfo a =
  TypeNameInfo
    { typeNameTopDir :: TopDir
    , typeNameModuleName :: Maybe ModuleName
    , typeNameTypeName   :: TypeName
    } deriving (Eq,Read,Show)

mkTypeNameInfo :: forall a . Arbitrary a => Typeable a => Settings -> Proxy a -> IO (TypeNameInfo a)
mkTypeNameInfo (Settings { useModuleNameAsSubDirectory
                       , goldenDirectoryOption}) proxy = do
  maybeModuleName <- maybeModuleNameIO
  return $ TypeNameInfo (TopDir         topDir )
                        (ModuleName <$> maybeModuleName )
                        (TypeName typeName)
  where
   typeName = show (typeRep proxy)
   maybeModuleNameIO =
     if useModuleNameAsSubDirectory
     then do
       arbA <- generate (arbitrary :: Gen a)
       return $ Just $ tyConModule . typeRepTyCon . typeOf $ arbA
     else return Nothing

   topDir =
     case goldenDirectoryOption of
       GoldenDirectory -> "golden"
       CustomDirectoryName d -> d

encodePrettySortedKeys :: ToJSON a => a -> ByteString
encodePrettySortedKeys = encodePretty' defConfig { confCompare = compare }
