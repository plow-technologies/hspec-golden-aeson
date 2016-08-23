{-# LANGUAGE OverloadedStrings #-}

module Test.Aeson.GenericSpecsSpec where

import           Control.Monad.IO.Class

import           Data.Aeson
import           Data.Proxy

import           GHC.Generics

import           System.Directory

import           Test.Aeson.GenericSpecs
import           Test.Hspec
import           Test.Hspec.Core.Runner
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.ADT

import           Test.Utils

-- various iterations of a Product and Sum Type and their serializations
import qualified Test.Types                     as T
import qualified Test.Types.AlteredSelector     as TAS
import qualified Test.Types.BrokenSerialization as TBS
import qualified Test.Types.MismatchedToAndFromSerialization as MTFS
import qualified Test.Types.NewSelector         as TNS


-- summaryFailures
spec :: Spec
spec = do
  describe "Test.Aeson.GenericSpecs: roundTrip" $ do
    it "should pass when ToJSON and FromJSON are defined appropriately" $ do
      (s1,_) <- hspecSilently $ roundtripADTSpecs (Proxy :: Proxy T.Person)
      summaryFailures s1 `shouldBe` 0

    it "should fail when ToJSON and FromJSON definitions do not match" $ do
      (s1,_) <- hspecSilently $ roundtripADTSpecs (Proxy :: Proxy MTFS.Person)
      summaryFailures s1 `shouldBe` 1

  describe "Test.Aeson.GenericSpecs: goldenADTSpecs" $ do
    it "create golden test files" $ do
      -- clean up previously existing golden folder
      bg <- doesDirectoryExist "golden"
      if bg
        then removeDirectoryRecursive "golden"
        else return ()

      -- files for Person and SumType do not exist
      -- create them by running goldenADTSpecs
      _ <- hspecSilently $ goldenADTSpecs defaultSettings (Proxy :: Proxy T.Person)
      _ <- hspecSilently $ goldenADTSpecs defaultSettings (Proxy :: Proxy T.SumType)

      doesFileExist "golden/Person/Person.json"    `shouldReturn` True
      doesFileExist "golden/SumType/SumType1.json" `shouldReturn` True
      doesFileExist "golden/SumType/SumType2.json" `shouldReturn` True
      doesFileExist "golden/SumType/SumType3.json" `shouldReturn` True


    it "create golden test files in a sub directory using the module name" $ do
      -- files for Person and SumType do not exist
      -- create them by running goldenADTSpecs
      _ <- hspecSilently $ goldenADTSpecs defaultSettings {useModuleNameAsSubDirectory = True } (Proxy :: Proxy T.Person)
      _ <- hspecSilently $ goldenADTSpecs defaultSettings {useModuleNameAsSubDirectory = True } (Proxy :: Proxy T.SumType)

      doesFileExist "golden/Test.Types/Person/Person.json"    `shouldReturn` True
      doesFileExist "golden/Test.Types/SumType/SumType1.json" `shouldReturn` True
      doesFileExist "golden/Test.Types/SumType/SumType2.json" `shouldReturn` True
      doesFileExist "golden/Test.Types/SumType/SumType3.json" `shouldReturn` True

    it "create golden test files in a user defined directory" $ do
      let topDir = "json-tests"
      -- clean up previously existing user defined folder
      bg <- doesDirectoryExist topDir
      if bg
        then removeDirectoryRecursive topDir
        else return ()

      -- files for Person and SumType do not exist
      -- create them by running goldenADTSpecs
      _ <- hspecSilently $ goldenADTSpecs (defaultSettings {goldenDirectoryOption = CustomDirectoryName topDir}) (Proxy :: Proxy T.Person)
      _ <- hspecSilently $ goldenADTSpecs (defaultSettings {goldenDirectoryOption = CustomDirectoryName topDir}) (Proxy :: Proxy T.SumType)

      doesFileExist "json-tests/Person/Person.json"    `shouldReturn` True
      doesFileExist "json-tests/SumType/SumType1.json" `shouldReturn` True
      doesFileExist "json-tests/SumType/SumType2.json" `shouldReturn` True
      doesFileExist "json-tests/SumType/SumType3.json" `shouldReturn` True

    it "goldenADTSpecs should pass for existing golden files in which model types and serialization have not changed" $ do
      (s1,_) <- hspecSilently $ goldenADTSpecs defaultSettings (Proxy :: Proxy T.Person)
      (s2,_) <- hspecSilently $ goldenADTSpecs defaultSettings (Proxy :: Proxy T.SumType)
      (summaryFailures s1 + summaryFailures s2) `shouldBe` 0

    it "goldenADTSpecs for types which have changed the values of ToJSON or FromJSON keys should fail to match the goldenFiles" $ do
      (s1,_) <- hspecSilently $ goldenADTSpecs defaultSettings (Proxy :: Proxy TBS.Person)
      summaryFailures s1 `shouldBe` 1

    it "goldenADTSpecs for types which have changed the values of ToJSON or FromJSON keys should fail to match the goldenFiles" $ do
      (s1,_) <- hspecSilently $ goldenADTSpecs defaultSettings (Proxy :: Proxy TNS.Person)
      summaryFailures s1 `shouldBe` 1

    it "goldenADTSpecs for types which have altered the name of the selector and using generic implementation of ToJSON and FromJSON should fail to match the goldenFiles" $ do
      (s1,_) <- hspecSilently $ goldenADTSpecs defaultSettings (Proxy :: Proxy TAS.Person)
      summaryFailures s1 `shouldBe` 1

main :: IO ()
main = hspec spec
