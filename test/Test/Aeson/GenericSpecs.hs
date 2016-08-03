{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Aeson.GenericSpecs where

import           Control.Monad.IO.Class

import           Data.Aeson
import           Data.Proxy

import           GHC.Generics

import           System.Directory

import           Test.Aeson.GenericSpecs
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.ADT


data Person = Person {
  name :: String
, age  :: Int
} deriving (Eq,Show,Generic)

instance ToJSON Person
instance FromJSON Person
instance ToADTArbitrary Person
instance Arbitrary Person where
  arbitrary = genericArbitrary

data SumType = SumType1 Int
             | SumType2 String Int
             | SumType3 Double String Int
  deriving (Eq,Show,Generic)

instance ToJSON SumType
instance FromJSON SumType
instance ToADTArbitrary SumType
instance Arbitrary SumType where
  arbitrary = genericArbitrary


{- delete golden directory
   run one test to create contents, check that contents are there
   run another test to make sure that tests happen

   test round robin
-}

spec :: Spec
spec =
  describe "tasty-golden-cereal" $ do
    it "create packages" $ do
      removeDirectoryRecursive "golden"
      liftIO $ print "create packages test"
      goldenTestWithNote (Proxy :: Proxy Person)
      goldenTestWithNote (Proxy :: Proxy SumType)
      False `shouldBe` True

    it "test existing golden files" $ do
      goldenTestWithNote (Proxy :: Proxy Person)
      goldenTestWithNote (Proxy :: Proxy SumType)
      False `shouldBe` True


main :: IO ()
main = hspec spec
