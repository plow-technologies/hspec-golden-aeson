{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Aeson.Internal.GoldenSpecs where

import           Control.Exception
import           Control.Monad

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy hiding (putStrLn)
import           Data.Proxy
import           Data.Typeable

import           Prelude hiding (readFile, writeFile)

import           System.Directory
import           System.FilePath
import           System.Random

import           Test.Aeson.Internal.RandomSamples
import           Test.Aeson.Internal.Utils
import           Test.Hspec
import           Test.QuickCheck

-- | Allows to obtain tests that will try to ensure that the JSON encoding
-- didn't change unintentionally. To this end 'goldenSpecs' will
--
-- - write a file @golden.json/TYPENAME.json@ in the current directory
--   containing a number of JSON-encoded sample values,
-- - during subsequent tests it will encode the same sample values again and
--   compare them with the saved golden encodings,
-- - on failure it will create a file @golden.json/TYPENAME.faulty.json@ for
--   easy manual inspection.
--
-- You can consider putting the golden files under revision control. That way
-- it'll be obvious when JSON encodings change.
goldenSpecs :: (Eq a, Show a, Typeable a, Arbitrary a, ToJSON a, FromJSON a) =>
  Settings -> Proxy a -> Spec
goldenSpecs settings proxy = goldenSpecsWithNote settings proxy Nothing

goldenSpecsWithNote :: forall a. (Eq a, Show a, Typeable a, Arbitrary a, ToJSON a, FromJSON a) =>
  Settings -> Proxy a -> Maybe String -> Spec
goldenSpecsWithNote settings@Settings{..} proxy mNote = do
  mModuleName <-
    if useModuleNameAsSubDirectory
      then return Nothing
      else do
        arbA <- runIO $ generate (arbitrary :: Gen a)
        return $ Just $ show . tyConModule . typeRepTyCon . typeOf $ arbA

  let goldenFile = mkGoldenFile topDir mModuleName proxy
      note = maybe "" (" " ++) mNote

  describe ("JSON encoding of " ++ addBrackets (show (typeRep proxy)) ++ note) $
    it ("produces the same JSON as is found in " ++ goldenFile) $ do
      exists <- doesFileExist goldenFile
      if exists
        then compareWithGolden topDir mModuleName proxy goldenFile
        else createGoldenfile settings proxy goldenFile
  where
    topDir = case goldenDirectoryOption of
      GoldenDirectory -> "golden"
      CustomDirectoryName d -> d

-- split on period, replace with </>
-- Data.List split

mkGoldenFile :: Typeable a => FilePath -> Maybe FilePath -> Proxy a -> FilePath
mkGoldenFile topDir mModuleName proxy =
  case mModuleName of
    Nothing         -> topDir </> show (typeRep proxy) <.> "json"
    Just moduleName -> topDir </> moduleName </> show (typeRep proxy) <.> "json"

mkFaultyFile :: Typeable a => FilePath -> Maybe FilePath -> Proxy a -> FilePath
mkFaultyFile topDir mModuleName proxy =
  case mModuleName of
    Nothing         -> topDir </> show (typeRep proxy) <.> "faulty" <.> "json"
    Just moduleName -> topDir </> moduleName </> show (typeRep proxy) <.> "faulty" <.> "json"

createGoldenfile :: forall a . (Show a, Arbitrary a, ToJSON a) =>
  Settings -> Proxy a -> FilePath -> IO ()
createGoldenfile Settings{..} proxy goldenFile = do
  createDirectoryIfMissing True (takeDirectory goldenFile)
  rSeed <- randomIO
  rSamples <- mkRandomSamples sampleSize proxy rSeed
  writeFile goldenFile (encodePretty rSamples)
  putStrLn $
    "\n" ++
    "WARNING: Running for the first time, not testing anything.\n" ++
    "  Created " ++ goldenFile ++ " containing random samples,\n" ++
    "  will compare JSON encodings with this from now on.\n" ++
    "  Please, consider putting " ++ goldenFile ++ " under version control."

compareWithGolden :: forall a .
  (Eq a, Show a, Typeable a, Arbitrary a, ToJSON a, FromJSON a) =>
  FilePath -> Maybe FilePath -> Proxy a -> FilePath -> IO ()
compareWithGolden topDir mModuleName proxy goldenFile = do
  goldenSeed <- readSeed =<< readFile goldenFile
  sampleSize <- readSampleSize =<< readFile goldenFile
  newSamples <- mkRandomSamples sampleSize proxy goldenSeed
  whenFails (writeComparisonFile newSamples) $ do
    goldenSamples :: RandomSamples a <-
      either (throwIO . ErrorCall) return =<<
      eitherDecode' <$>
      readFile goldenFile
    newSamples `shouldBe` goldenSamples
  where
    whenFails :: forall b c . IO c -> IO b -> IO b
    whenFails = flip onException

    faultyFile = mkFaultyFile topDir mModuleName proxy

    writeComparisonFile newSamples = do  
      writeFile faultyFile (encodePretty newSamples)
      putStrLn $
        "\n" ++
        "INFO: Written the current encodings into " ++ faultyFile ++ "."

mkRandomSamples :: forall a . Arbitrary a =>
  Int -> Proxy a -> Int -> IO (RandomSamples a)
mkRandomSamples sampleSize Proxy rSeed = RandomSamples rSeed <$> generate gen
  where
    correctedSampleSize = if sampleSize <= 0 then 1 else sampleSize
    gen :: Gen [a]
    gen = setSeed rSeed $ replicateM correctedSampleSize (arbitrary :: Gen a)
