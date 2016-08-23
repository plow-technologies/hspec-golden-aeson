{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Aeson.Internal.ADT.GoldenSpecs where

import           Control.Arrow
import           Control.Exception
import           Control.Monad

import           Data.Aeson                (ToJSON, FromJSON)
import qualified Data.Aeson                as A
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy      (writeFile, readFile)
import           Data.Proxy

import           Prelude            hiding (writeFile,readFile)

import           System.Directory
import           System.FilePath
import           System.Random

import           Test.Aeson.Internal.RandomSamples
import           Test.Aeson.Internal.Utils
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.ADT

-- | for a type a, create a set of golden files if they do not exist, compare
-- with golden file if it exists. Golden file encodes json format of a type
goldenADTSpecs :: forall a. (ToADTArbitrary a, Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a) =>
  Settings -> Proxy a -> Spec
goldenADTSpecs settings proxy = goldenADTSpecsWithNote settings proxy Nothing

goldenADTSpecsWithNote :: forall a. (ToADTArbitrary a, Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a) =>
  Settings -> Proxy a -> Maybe String -> Spec
goldenADTSpecsWithNote settings Proxy mNote = do
  (moduleName,(typeName,constructors)) <- runIO $ fmap (adtModuleName &&& adtTypeName &&& adtCAPs) <$> generate $ toADTArbitrary (Proxy :: Proxy a)
  describe ("JSON encoding of " ++ typeName ++ note) $
    mapM_ (testConstructor settings moduleName typeName) constructors
  where
    note = maybe "" (" " ++) mNote

testConstructor :: forall a. (Eq a, Show a, FromJSON a, ToJSON a, ToADTArbitrary a) =>
  Settings -> String -> String -> ConstructorArbitraryPair a -> SpecWith ( Arg (IO ()))
testConstructor Settings{..} moduleName typeName cap = do
  it ("produces the same JSON as is found in " ++ goldenFile) $ do
    exists <- doesFileExist goldenFile
    if exists
      then compareWithGolden topDir mModuleName typeName cap goldenFile
      else createGoldenFile sampleSize cap goldenFile
  where
    goldenFile = mkGoldenFilePath topDir mModuleName typeName cap
    topDir = case goldenDirectoryOption of
      GoldenDirectory -> "golden"
      CustomDirectoryName d -> d
    mModuleName = case useModuleNameAsSubDirectory of
      True  -> Just moduleName
      False -> Nothing

compareWithGolden :: forall a. (Show a, Eq a, FromJSON a, ToJSON a, ToADTArbitrary a) =>
  String -> Maybe String -> String -> ConstructorArbitraryPair a -> FilePath -> IO ()
compareWithGolden topDir mModuleName typeName cap goldenFile = do
  goldenSeed <- readSeed =<< readFile goldenFile
  sampleSize <- readSampleSize =<< readFile goldenFile
  newSamples <- mkRandomADTSamplesForConstructor sampleSize (Proxy :: Proxy a) (capConstructor cap) goldenSeed
  whenFails (writeComparisonFile newSamples) $ do
    goldenSamples :: RandomSamples a <-
      either (throwIO . ErrorCall) return =<<
      A.eitherDecode' <$>
      readFile goldenFile
    newSamples `shouldBe` goldenSamples
  where
    whenFails :: forall b c. IO c -> IO b -> IO b
    whenFails = flip onException

    faultyFile = mkFaultyFilePath topDir mModuleName typeName cap

    writeComparisonFile newSamples = do
      writeFile faultyFile (encodePretty newSamples)
      putStrLn $
        "\n" ++
        "INFO: Written the current encodings into " ++ faultyFile ++ "."

createGoldenFile :: forall a. (ToJSON a, ToADTArbitrary a) =>
  Int -> ConstructorArbitraryPair a -> FilePath -> IO ()
createGoldenFile sampleSize cap goldenFile = do
  createDirectoryIfMissing True (takeDirectory goldenFile)
  rSeed <- randomIO :: IO Int
  rSamples <- mkRandomADTSamplesForConstructor sampleSize (Proxy :: Proxy a) (capConstructor cap) rSeed
  writeFile goldenFile $ encodePretty rSamples

  putStrLn $
    "\n" ++
    "WARNING: Running for the first time, not testing anything.\n" ++
    "  Created " ++ goldenFile ++ " containing random samples,\n" ++
    "  will compare JSON encodings with this from now on.\n" ++
    "  Please, consider putting " ++ goldenFile ++ " under version control."


mkGoldenFilePath :: forall a. FilePath -> Maybe FilePath -> FilePath -> ConstructorArbitraryPair a -> FilePath
mkGoldenFilePath topDir mModuleName typeName cap =
  case mModuleName of
    Nothing -> topDir </> typeName </> capConstructor cap <.> "json"
    Just moduleName -> topDir </> moduleName </> typeName </> capConstructor cap <.> "json"

mkFaultyFilePath :: forall a. FilePath -> Maybe FilePath -> FilePath -> ConstructorArbitraryPair a -> FilePath
mkFaultyFilePath topDir mModuleName typeName cap =
  case mModuleName of
    Nothing -> topDir </> typeName </> capConstructor cap <.> "faulty" <.> "json"
    Just moduleName -> topDir </> moduleName </> typeName </> capConstructor cap <.> "faulty" <.> "json"

mkRandomADTSamplesForConstructor :: forall a. (ToADTArbitrary a) =>
  Int -> Proxy a -> String -> Int -> IO (RandomSamples a)
mkRandomADTSamplesForConstructor sampleSize Proxy conName rSeed = do
  generatedADTs <- generate gen
  let caps         = concat $ adtCAPs <$> generatedADTs
      filteredCAPs = filter (\x -> capConstructor x == conName) caps
      arbs         = capArbitrary <$> filteredCAPs
  return $ RandomSamples rSeed arbs
  where
    correctedSampleSize = if sampleSize <= 0 then 1 else sampleSize
    gen = setSeed rSeed $ replicateM correctedSampleSize (toADTArbitrary (Proxy :: Proxy a))
