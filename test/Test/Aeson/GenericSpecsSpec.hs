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
import qualified Test.Types as T
import qualified Test.Types.BrokenSerialization as TBS
import qualified Test.Types.NewSelector as TNS
import qualified Test.Types.AlteredSelector as TAS


{- delete golden directory
   run one test to create contents, check that contents are there
   run another test to make sure that tests happen

   test round robin
-}

-- summaryFailures
spec :: Spec
spec = do
  describe "Test.Aeson.GenericSpecs: roundTrip" $
    it "" $ do
      (s1,_) <- hspecSilently $ constructorRoundtripSpecs (Proxy :: Proxy T.Person)
      summaryFailures s1 `shouldBe` 0

  describe "Test.Aeson.GenericSpecs: goldenTest" $ do
    it "create golden test files" $ do
      -- clean up previously existing golden folder
      removeDirectoryRecursive "golden"

      -- files for Person and SumType do not exist
      -- create them by running goldenTest
      _ <- hspecSilently $ goldenTest (Proxy :: Proxy T.Person)
      _ <- hspecSilently $ goldenTest (Proxy :: Proxy T.SumType)

      doesFileExist "golden/Person/Person.json"    `shouldReturn` True
      doesFileExist "golden/SumType/SumType1.json" `shouldReturn` True
      doesFileExist "golden/SumType/SumType2.json" `shouldReturn` True
      doesFileExist "golden/SumType/SumType3.json" `shouldReturn` True

    it "goldenTest should pass for existing golden files in which model types and serialization have not changed" $ do
      (s1,_) <- hspecSilently $ goldenTest (Proxy :: Proxy T.Person)
      (s2,_) <- hspecSilently $ goldenTest (Proxy :: Proxy T.SumType)
      (summaryFailures s1 + summaryFailures s2) `shouldBe` 0

    it "goldenTest for types which have changed the values of ToJSON or FromJSON keys should fail to match the goldenFiles" $ do
      (s1,_) <- hspecSilently $ goldenTest (Proxy :: Proxy TBS.Person)
      summaryFailures s1 `shouldBe` 1

    it "goldenTest for types which have changed the values of ToJSON or FromJSON keys should fail to match the goldenFiles" $ do
      (s1,_) <- hspecSilently $ goldenTest (Proxy :: Proxy TNS.Person)
      summaryFailures s1 `shouldBe` 1

    it "goldenTest for types which have altered the name of the selector and using generic implementation of ToJSON and FromJSON should fail to match the goldenFiles" $ do
      (s1,_) <- hspecSilently $ goldenTest (Proxy :: Proxy TAS.Person)
      summaryFailures s1 `shouldBe` 1



main :: IO ()
main = hspec spec
