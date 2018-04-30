{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Types.BrokenSerialization where

import           Data.Aeson
import           GHC.Generics
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.ADT

data Person = Person {
  name :: String
, age  :: Int
} deriving (Eq,Show,Generic)

instance ToJSON Person where
  toJSON (Person name' age') = object [
       "personName" .= name'
    ,  "personAge"  .= age'
    ]
instance FromJSON Person where
  parseJSON = withObject "Expected a Person object" $ \o ->
    Person <$> o .: "personName"
           <*> o .: "personAge"

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
