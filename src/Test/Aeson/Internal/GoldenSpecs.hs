{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Aeson.Internal.GoldenSpecs where

import           Control.Arrow
import           Control.Exception
import           Control.Monad

import           Data.Aeson                (ToJSON, FromJSON,Value)
import qualified Data.Aeson                as A
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy      (ByteString,writeFile,readFile)
import           Data.Proxy

import           GHC.Generics

import           Prelude            hiding (writeFile,readFile)

import           System.Directory
import           System.FilePath
import           System.Random

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.ADT
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Random


someFunc :: IO ()
someFunc = putStrLn "someFunc"

{-
createGoldenfile :: forall a . (Show a, Arbitrary a, ToJSON a) =>
  Proxy a -> FilePath -> IO ()
createGoldenfile proxy goldenFile = do
  createDirectoryIfMissing True (takeDirectory goldenFile)
  seed <- randomIO
  samples <- mkRandomSamples proxy seed
  writeFile goldenFile (encodePretty samples)
  putStrLn $
    "\n" ++
    "WARNING: Running for the first time, not testing anything.\n" ++
    "  Created " ++ goldenFile ++ " containing random samples,\n" ++
    "  will compare JSON encodings with this from now on.\n" ++
"  Please, consider putting " ++ goldenFile ++ " under version control."
-}
goldenTest :: forall a. (Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a, ToADTArbitrary a)
           => Proxy a
           -> Spec
goldenTest proxy =
  describe "JSON encoding of" $
    it "produces the smae JSON as is found in " $ do
      (typeName,constructors) <- fmap (_adtTypeName &&& _adtCAPs) <$> generate $ toADTArbitrary (Proxy :: Proxy a)
      mapM_ (testConstructor typeName) constructors


testConstructor :: forall a. (Eq a, Show a, FromJSON a, ToJSON a, ToADTArbitrary a)
                => String
                -> ConstructorArbitraryPair a
                -> IO ()
testConstructor typeName cap = do
  let goldenFile = mkGoldenFilePath typeName cap
  exists <- doesFileExist goldenFile
  if exists
    then compareWithGolden typeName cap goldenFile
    else createGoldenFile cap goldenFile


compareWithGolden :: forall a. (Show a, Eq a, FromJSON a, ToJSON a, ToADTArbitrary a)
                  => String
                  -> ConstructorArbitraryPair a
                  -> FilePath
                  -> IO ()
compareWithGolden typeName cap goldenFile = do
  goldenSeed <- readSeed =<< readFile goldenFile
  newSamples <- mkRandomADTSamplesForConstructor (Proxy :: Proxy a) (_capConstructor cap) goldenSeed

  whenFails (writeComparisonFile newSamples) $ do
    goldenSamples :: RandomSamples a <-
      either (throwIO . ErrorCall) return =<<
      A.eitherDecode' <$>
      readFile goldenFile
    newSamples `shouldBe` goldenSamples
  return ()
  where
    whenFails :: forall a b. IO b -> IO a -> IO a
    whenFails = flip onException
    faultyFile = mkFaultyFilePath typeName cap
    writeComparisonFile newSamples = do
      writeFile faultyFile (encodePretty newSamples)
      putStrLn $
        "\n" ++
        "INFO: Written the current encodings into " ++ faultyFile ++ "."

createGoldenFile :: forall a. (ToJSON a, ToADTArbitrary a) => ConstructorArbitraryPair a -> FilePath -> IO ()
createGoldenFile cap goldenFile = do
  createDirectoryIfMissing True (takeDirectory goldenFile)
  seed <- randomIO :: IO Int
  samples <- mkRandomADTSamplesForConstructor (Proxy :: Proxy a) (_capConstructor cap) seed
  writeFile goldenFile $ encodePretty samples

  putStrLn $
    "\n" ++
    "WARNING: Running for the first time, not testing anything.\n" ++
    "  Created " ++ goldenFile ++ " containing random samples,\n" ++
    "  will compare JSON encodings with this from now on.\n" ++
    "  Please, consider putting " ++ goldenFile ++ " under version control."

mkGoldenFilePath :: forall a. String -> ConstructorArbitraryPair a -> FilePath
mkGoldenFilePath typeName cap = "golden" </> typeName </> _capConstructor cap <.> "json"

mkFaultyFilePath :: forall a. String -> ConstructorArbitraryPair a -> FilePath
mkFaultyFilePath typeName cap = "golden" </> typeName </> _capConstructor cap <.> "faulty" <.> "json"

{-
data ADTArbitrary a = ADTArbitrary {
  _adtTypeName :: String
, _adtCAPs     :: [ConstructorArbitraryPair a]
} deriving (Eq,Generic,Read,Show,Typeable)

data ConstructorArbitraryPair a = ConstructorArbitraryPair {
  _capConstructor :: String
, _capArbitrary   :: a
} deriving (Eq,Generic,Read,Show,Typeable)

-}

-- | RandomSamples, using a seed allows you to replicate an arbitrary. By
-- storing the seed and the samples (previously produced arbitraries), we can
-- try to reproduce the same samples by generating the arbitraries with a seed
--

data RandomSamples a = RandomSamples {
  seed    :: Int
, samples :: [a]
} deriving (Eq, Ord, Show, Generic)

instance FromJSON a => FromJSON (RandomSamples a)
instance ToJSON   a => ToJSON   (RandomSamples a)

mkRandomSamples :: forall a.
                   Arbitrary a
                => Proxy a
                -> Int
                -> IO (RandomSamples a)
mkRandomSamples Proxy seed = RandomSamples seed <$> generate gen
  where
    gen :: Gen [a]
    gen = setSeed seed $ replicateM 200 (arbitrary :: Gen a)

mkRandomADTSamplesForConstructor :: forall a.
                                    ToADTArbitrary a
                                 => Proxy a
                                 -> String
                                 -> Int
                                 -> IO (RandomSamples a)
mkRandomADTSamplesForConstructor Proxy conName seed = do
  generatedADTs <- generate gen
  let caps         = concat $ _adtCAPs <$> generatedADTs
      filteredCAPs = filter (\x -> _capConstructor x == conName) caps
      arbs         = _capArbitrary <$> filteredCAPs
  return $ RandomSamples seed arbs
  where
    gen = setSeed seed $ replicateM 200 (toADTArbitrary (Proxy :: Proxy a))

setSeed :: Int -> Gen a -> Gen a
setSeed seed (MkGen g) = MkGen $ \ _randomSeed size -> g (mkQCGen seed) size

-- reads the seed without looking at the samples
readSeed :: ByteString -> IO Int
readSeed s = case A.eitherDecode s :: Either String (RandomSamples Value) of
  Right samples -> return $ seed samples
  Left err -> throwIO $ ErrorCall err
