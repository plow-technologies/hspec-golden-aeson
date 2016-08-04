{-# LANGUAGE DeriveGeneric #-}

module Test.Types.AlteredSelector where

import           Data.Aeson
import           GHC.Generics
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.ADT

data Person = Person {
  name   :: String
, height :: Int
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
