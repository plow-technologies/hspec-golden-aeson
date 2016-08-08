{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Aeson.Internal.GoldenSpecs where

import           Control.Exception
import           Control.Monad

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy hiding (putStrLn)
import           Data.Proxy
import           Data.Typeable

import           GHC.Generics

import           Prelude hiding (readFile, writeFile)

import           System.Directory
import           System.FilePath
import           System.Random

import           Test.Aeson.Internal.RandomSamples
import           Test.Aeson.Internal.Utils
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Random

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
  Proxy a -> Spec
goldenSpecs proxy = goldenSpecsWithNote proxy Nothing

goldenSpecsWithNote :: (Eq a, Show a, Typeable a, Arbitrary a, ToJSON a, FromJSON a) =>
  Proxy a -> Maybe String -> Spec
goldenSpecsWithNote proxy mNote = do
  let goldenFile = mkGoldenFile proxy
      note = maybe "" (" " ++) mNote
  describe ("JSON encoding of " ++ addBrackets (show (typeRep proxy)) ++ note) $
    it ("produces the same JSON as is found in " ++ goldenFile) $ do
      exists <- doesFileExist goldenFile
      if exists
        then compareWithGolden proxy goldenFile
        else createGoldenfile proxy goldenFile

mkGoldenFile :: Typeable a => Proxy a -> FilePath
mkGoldenFile proxy =
  "golden.json" </> show (typeRep proxy) <.> "json"

mkFaultyFile :: Typeable a => Proxy a -> FilePath
mkFaultyFile proxy =
  "golden.json" </> show (typeRep proxy) <.> "faulty" <.> "json"

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

compareWithGolden :: forall a .
  (Eq a, Show a, Typeable a, Arbitrary a, ToJSON a, FromJSON a) =>
  Proxy a -> FilePath -> IO ()
compareWithGolden proxy goldenFile = do
  goldenSeed <- readSeed =<< readFile goldenFile
  newSamples <- mkRandomSamples proxy goldenSeed
  whenFails (writeComparisonFile newSamples) $ do
    goldenSamples :: RandomSamples a <-
      either (throwIO . ErrorCall) return =<<
      eitherDecode' <$>
      readFile goldenFile
    newSamples `shouldBe` goldenSamples
  where
    whenFails :: forall a b . IO b -> IO a -> IO a
    whenFails = flip onException

    writeComparisonFile newSamples = do
      writeFile (mkFaultyFile proxy) (encodePretty newSamples)
      putStrLn $
        "\n" ++
        "INFO: Written the current encodings into " ++ mkFaultyFile proxy ++ "."

mkRandomSamples :: forall a . Arbitrary a =>
  Proxy a -> Int -> IO (RandomSamples a)
mkRandomSamples Proxy seed = RandomSamples seed <$> generate gen
  where
    gen :: Gen [a]
    gen = setSeed seed $ replicateM 5 (arbitrary :: Gen a)
