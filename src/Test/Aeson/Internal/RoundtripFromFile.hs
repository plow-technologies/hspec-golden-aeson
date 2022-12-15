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
module Test.Aeson.Internal.RoundtripFromFile where

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
import           System.FilePath

import           Test.Aeson.Internal.Utils
import           Test.Hspec
import           Test.HUnit.Lang (HUnitFailure)
import           Test.QuickCheck
import           Test.Aeson.Internal.RandomSamples
import           Test.Aeson.Internal.GoldenSpecs


roundtripFromFile :: forall a. 
  (Typeable a, Arbitrary a, Eq a, Show a, ToJSON a, FromJSON a)
  => Settings -> Proxy a -> Spec
roundtripFromFile settings proxy = do
  typeNameInfo    <- runIO $ mkTypeNameInfo settings proxy
  let goldenFile = mkGoldenFile typeNameInfo

  createMissing <- runIO $ isJust <$> lookupEnv "CREATE_MISSING_GOLDEN"
  let fixIfFlag err = do
        doFix <- isJust <$> lookupEnv "RECREATE_BROKEN_GOLDEN"
        if doFix then do
          createGoldenfile settings proxy goldenFile
        else 
          throwIO err
  describe ("JSON encoding of " ++ addBrackets  (unTypeName $ typeNameTypeName typeNameInfo)) $
    it ("produces the same JSON as is found in " ++ goldenFile) $ do

      fileExist <- doesFileExist goldenFile
      if fileExist then
        runTestFromFile proxy goldenFile 
          `catches` 
            [ 
              Handler (\(err :: HUnitFailure) -> fixIfFlag err), 
              Handler (\(err :: AesonDecodeError) -> fixIfFlag err)
            ]
      else if createMissing then
        createGoldenfile settings proxy goldenFile
      else
        expectationFailure $ "Missing golden file: " ++ goldenFile

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