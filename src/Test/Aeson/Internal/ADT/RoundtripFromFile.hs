{-|
Module      : Test.Aeson.Internal.RoundtripFromFile
Description : Golden tests for Arbitrary
Copyright   : (c) Plow Technologies, 2022
License     : BSD3
Maintainer  : bruno.cadorette@plowtech.net
Stability   : Beta

Internal module, use at your own risk.
-}

{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
module Test.Aeson.Internal.ADT.RoundtripFromFile where

import           Control.Exception
import           Control.Monad

import           Data.Aeson
import           Data.ByteString.Lazy hiding (putStrLn)
import           Data.Maybe (isJust)
import           Data.Proxy
import           Data.Typeable

import           Prelude hiding (readFile, writeFile)

import           System.Directory
import           System.Environment (lookupEnv)

import           Test.Aeson.Internal.Utils
import           Test.Hspec
import           Test.HUnit.Lang (HUnitFailure)
import           Test.Aeson.Internal.RandomSamples
import           Test.Aeson.Internal.ADT.GoldenSpecs
import           Test.QuickCheck.Arbitrary.ADT



roundtripADTFromFile :: forall a. 
  (Typeable a, ToADTArbitrary a, Eq a, Show a, ToJSON a, FromJSON a)
  => Settings -> Proxy a -> Spec
roundtripADTFromFile settings proxy = do
  (moduleName,typeName,constructors) <- runIO $ generateInfoFromADT proxy
  describe ("JSON encoding of " ++ typeName) $
    mapM_ (testConstructorRoundtrip settings moduleName typeName) constructors

testConstructorRoundtrip :: forall a. (Eq a, Show a, FromJSON a, ToJSON a, ToADTArbitrary a) =>
  Settings -> String -> String -> ConstructorArbitraryPair a -> SpecWith ( Arg (IO ()))
testConstructorRoundtrip Settings{..} moduleName typeName cap = do
  it ("produces the same JSON as is found in " ++ goldenFile) $ do
    createMissing <- isJust <$> lookupEnv "CREATE_MISSING_GOLDEN"
    fileExist <- doesFileExist goldenFile
    if fileExist then
      runTestFromFile (Proxy @a) goldenFile 
        `catches` 
          [ 
            Handler (\(err :: HUnitFailure) -> fixIfFlag err), 
            Handler (\(err :: AesonDecodeError) -> fixIfFlag err)
          ]
    else if createMissing then
      createGoldenFile sampleSize cap goldenFile
    else
      expectationFailure $ "Missing golden file: " ++ goldenFile

  where
    goldenFile = mkGoldenFilePath topDir mModuleName typeName cap
    topDir = case goldenDirectoryOption of
      GoldenDirectory -> "golden"
      CustomDirectoryName d -> d
    mModuleName = case useModuleNameAsSubDirectory of
      True  -> Just moduleName
      False -> Nothing
    
    fixIfFlag err = do
      doFix <- isJust <$> lookupEnv "RECREATE_BROKEN_GOLDEN"
      if doFix then do
        createGoldenFile sampleSize cap goldenFile
      else 
        throwIO err

runTestFromFile :: forall a . (Eq a, Show a, ToJSON a, FromJSON a) => Proxy a -> String -> IO ()
runTestFromFile _proxy goldenFile = do
  bytes <- readFile goldenFile
  case decode bytes of
    Just (randomSamples :: RandomSamples Value) -> 
      let jsonList = samples randomSamples in
      case traverse fromJSON jsonList  of
        Data.Aeson.Success (decodedData :: [a]) ->
          fmap toJSON decodedData `shouldBe` jsonList
        Data.Aeson.Error err -> expectationFailure err
    Nothing -> expectationFailure "Cannot decode JSON"