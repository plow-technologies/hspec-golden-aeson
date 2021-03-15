{-|
Module      : Test.Aeson.Internal.ADT.GoldenSpecs
Description : Golden tests for ToADTArbitrary
Copyright   : (c) Plow Technologies, 2016
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : Beta

Internal module, use at your own risk.
-}

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
import           Data.ByteString.Lazy      (writeFile, readFile)
import           Data.Int                  (Int32)
import           Data.Maybe                (isJust)
import           Data.Proxy

import           Prelude            hiding (writeFile,readFile)

import           System.Directory
import           System.Environment        (lookupEnv)
import           System.FilePath
import           System.Random

import           Test.Aeson.Internal.RandomSamples
import           Test.Aeson.Internal.Utils
import           Test.Hspec
import           Test.HUnit.Lang (HUnitFailure)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.ADT

-- | Tests to ensure that JSON encoding has not unintentionally changed. This
-- could be caused by the following:
--
-- - A type's instances of `ToJSON` or 'FromJSON' have changed.
-- - Selectors have been edited, added or deleted.
-- - You have changed version of Aeson the way Aeson serialization has changed
--   works.
--
-- If you run this function and the golden files do not
-- exist, it will create them for each constructor. It they do exist, it will
-- compare with golden file if it exists. Golden file encodes json format of a
-- type. It is recommended that you put the golden files under revision control
-- to help monitor changes.
goldenADTSpecs :: forall a. (ToADTArbitrary a, Eq a, Show a, ToJSON a, FromJSON a) =>
  Settings -> Proxy a -> Spec
goldenADTSpecs settings proxy = goldenADTSpecsWithNote settings proxy Nothing

-- | same as 'goldenADTSpecs' but has the option of passing a note to the
-- 'describe' function.
goldenADTSpecsWithNote :: forall a. (ToADTArbitrary a, Eq a, Show a, ToJSON a, FromJSON a) =>
  Settings -> Proxy a -> Maybe String -> Spec
goldenADTSpecsWithNote settings Proxy mNote = do
  (moduleName,(typeName,constructors)) <- runIO $ fmap (adtModuleName &&& adtTypeName &&& adtCAPs) <$> generate $ toADTArbitrary (Proxy :: Proxy a)
  describe ("JSON encoding of " ++ typeName ++ note) $
    mapM_ (testConstructor settings moduleName typeName) constructors
  where
    note = maybe "" (" " ++) mNote

-- | test a single set of values from a constructor for a given type.
testConstructor :: forall a. (Eq a, Show a, FromJSON a, ToJSON a, ToADTArbitrary a) =>
  Settings -> String -> String -> ConstructorArbitraryPair a -> SpecWith ( Arg (IO ()))
testConstructor Settings{..} moduleName typeName cap =
  it ("produces the same JSON as is found in " ++ goldenFile) $ do
    exists <- doesFileExist goldenFile
    let fixIfFlag err = do
          doFix <- isJust <$> lookupEnv "RECREATE_BROKEN_GOLDEN"
          if doFix
            then createGoldenFile sampleSize cap goldenFile
            else throwIO err
    if exists
      then compareWithGolden randomMismatchOption topDir mModuleName typeName cap goldenFile
        `catches` [ Handler (\(err :: HUnitFailure) -> fixIfFlag err)
                  , Handler (\(err :: AesonDecodeError) -> fixIfFlag err)
                  ]
      else do
        doCreate <- isJust <$> lookupEnv "CREATE_MISSING_GOLDEN"
        if doCreate
          then createGoldenFile sampleSize cap goldenFile
          else expectationFailure $ "Missing golden file: " <> goldenFile
  where
    goldenFile = mkGoldenFilePath topDir mModuleName typeName cap
    topDir = case goldenDirectoryOption of
      GoldenDirectory -> "golden"
      CustomDirectoryName d -> d
    mModuleName = case useModuleNameAsSubDirectory of
      True  -> Just moduleName
      False -> Nothing

-- | The golden files already exist. Serialize values with the same seed from
-- the golden files of each constructor and compare.
compareWithGolden :: forall a. (Show a, Eq a, FromJSON a, ToJSON a, ToADTArbitrary a) =>
  RandomMismatchOption -> String -> Maybe String -> String -> ConstructorArbitraryPair a -> FilePath -> IO ()
compareWithGolden randomOption topDir mModuleName typeName cap goldenFile = do
  goldenSeed <- readSeed =<< readFile goldenFile
  sampleSize <- readSampleSize =<< readFile goldenFile
  newSamples <- mkRandomADTSamplesForConstructor sampleSize (Proxy :: Proxy a) (capConstructor cap) goldenSeed
  whenFails (writeComparisonFile newSamples) $ do
    goldenBytes <- readFile goldenFile
    goldenSamples :: RandomSamples a <- aesonDecodeIO goldenBytes
    if newSamples == goldenSamples
      then
        -- random samples match; test encoding of samples (the above check only tested the decoding)
        encodePrettySortedKeys newSamples == goldenBytes `shouldBe` True
      else do
        let
          -- whether to pass the test or fail due to random value mismatch
          finalResult =
            case randomOption of
              RandomMismatchWarning -> return ()
              RandomMismatchError -> expectationFailure "New random samples generated from seed in golden file do not match samples in golden file."

        -- do a fallback test to determine whether the mismatch is due to a random sample change only,
        -- or due to a change in encoding
        putStrLn $
          "\n" ++
          "WARNING: New random samples do not match those in " ++ goldenFile ++ ".\n" ++
          "  Testing round-trip decoding/encoding of golden file."
        let reencodedGoldenSamples = encodePrettySortedKeys goldenSamples
        if reencodedGoldenSamples == goldenBytes
          then
            -- pass the test because round-trip decode/encode still gives the same bytes
            finalResult
          else do
            -- how significant is the serialization change?
            writeReencodedComparisonFile goldenSamples
            testSamples :: RandomSamples a <- aesonDecodeIO reencodedGoldenSamples
            let
              failureMessage =
                if testSamples == goldenSamples
                  then
                    "Encoding has changed in a minor way; still can read old encodings. See " ++ faultyReencodedFile ++ "."
                  else
                    "Encoding has changed in a major way; cannot read old encodings. See " ++ faultyReencodedFile ++ "."
            expectationFailure failureMessage
            finalResult
  where
    whenFails :: forall b c. IO c -> IO b -> IO b
    whenFails = flip onException

    faultyFile = mkFaultyFilePath topDir mModuleName typeName cap
    faultyReencodedFile = mkFaultyReencodedFilePath topDir mModuleName typeName cap

    writeComparisonFile newSamples = do
      writeFile faultyFile (encodePrettySortedKeys newSamples)
      putStrLn $
        "\n" ++
        "INFO: Written the current encodings into " ++ faultyFile ++ "."
    writeReencodedComparisonFile samples = do
      writeFile faultyReencodedFile (encodePrettySortedKeys samples)
      putStrLn $
        "\n" ++
        "INFO: Written the re-encodings into " ++ faultyReencodedFile ++ "."

-- | The golden files do not exist. Create them for each constructor.
createGoldenFile :: forall a. (ToJSON a, ToADTArbitrary a) =>
  Int -> ConstructorArbitraryPair a -> FilePath -> IO ()
createGoldenFile sampleSize cap goldenFile = do
  createDirectoryIfMissing True (takeDirectory goldenFile)
  rSeed <- randomIO :: IO Int32
  rSamples <- mkRandomADTSamplesForConstructor sampleSize (Proxy :: Proxy a) (capConstructor cap) rSeed
  writeFile goldenFile $ encodePrettySortedKeys rSamples

  putStrLn $
    "\n" ++
    "WARNING: Running for the first time, not testing anything.\n" ++
    "  Created " ++ goldenFile ++ " containing random samples,\n" ++
    "  will compare JSON encodings with this from now on.\n" ++
    "  Please, consider putting " ++ goldenFile ++ " under version control."

-- | Create the file path for the golden file. Optionally use the module name to
-- help avoid name collissions. Different modules can have types of the same
-- name.
mkGoldenFilePath :: forall a. FilePath -> Maybe FilePath -> FilePath -> ConstructorArbitraryPair a -> FilePath
mkGoldenFilePath topDir mModuleName typeName cap =
  case mModuleName of
    Nothing -> topDir </> typeName </> capConstructor cap <.> "json"
    Just moduleName -> topDir </> moduleName </> typeName </> capConstructor cap <.> "json"

-- | Create the file path to save results from a failed golden test. Optionally
-- use the module name to help avoid name collisions.  Different modules can
-- have types of the same name.
mkFaultyFilePath :: forall a. FilePath -> Maybe FilePath -> FilePath -> ConstructorArbitraryPair a -> FilePath
mkFaultyFilePath topDir mModuleName typeName cap =
  case mModuleName of
    Nothing -> topDir </> typeName </> capConstructor cap <.> "faulty" <.> "json"
    Just moduleName -> topDir </> moduleName </> typeName </> capConstructor cap <.> "faulty" <.> "json"

-- | Create the file path to save results from a failed fallback golden test. Optionally
-- use the module name to help avoid name collisions.  Different modules can
-- have types of the same name.
mkFaultyReencodedFilePath :: forall a. FilePath -> Maybe FilePath -> FilePath -> ConstructorArbitraryPair a -> FilePath
mkFaultyReencodedFilePath topDir mModuleName typeName cap =
  case mModuleName of
    Nothing -> topDir </> typeName </> capConstructor cap <.> "faulty" <.> "reencoded" <.> "json"
    Just moduleName -> topDir </> moduleName </> typeName </> capConstructor cap <.> "faulty" <.> "reencoded" <.> "json"

-- | Create a number of arbitrary instances of a particular constructor given
-- a sample size and a random seed.
mkRandomADTSamplesForConstructor :: forall a. (ToADTArbitrary a) =>
  Int -> Proxy a -> String -> Int32 -> IO (RandomSamples a)
mkRandomADTSamplesForConstructor sampleSize Proxy conName rSeed = do
  generatedADTs <- generate gen
  let caps         = concat $ adtCAPs <$> generatedADTs
      filteredCAPs = filter (\x -> capConstructor x == conName) caps
      arbs         = capArbitrary <$> filteredCAPs
  return $ RandomSamples rSeed arbs
  where
    correctedSampleSize = if sampleSize <= 0 then 1 else sampleSize
    gen = setSeed (fromIntegral rSeed) $ replicateM correctedSampleSize (toADTArbitrary (Proxy :: Proxy a))

-- | Make a Golden File for the Proxy of a type if the file does not exist.
mkGoldenFileForType :: forall a. (ToJSON a, ToADTArbitrary a) => Int -> Proxy a -> FilePath -> IO ()
mkGoldenFileForType sampleSize Proxy goldenPath = do
  (typeName, constructors) <- fmap (adtTypeName &&& adtCAPs) <$> generate $ toADTArbitrary (Proxy :: Proxy a)
  mapM_
    (\constructor -> do
        let goldenFile = goldenPath </> typeName </> capConstructor constructor <.> ".json"
        exists <- doesFileExist goldenFile
        if exists
          then pure ()
          else do
            createDirectoryIfMissing True (takeDirectory goldenFile)
            rSeed <- randomIO :: IO Int32
            rSamples <- mkRandomADTSamplesForConstructor sampleSize (Proxy :: Proxy a) (capConstructor constructor) rSeed
            writeFile goldenFile $ encodePrettySortedKeys rSamples
    ) constructors
