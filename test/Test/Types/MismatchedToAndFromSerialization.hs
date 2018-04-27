{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Types.MismatchedToAndFromSerialization where

import           Data.Aeson
import           GHC.Generics
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.ADT

data Person = Person {
  name :: String
, age  :: Int
} deriving (Eq,Show,Generic)

-- ToJSON and FromJSON use different strings, this should break.
instance ToJSON Person where
  toJSON (Person name' age') = object [
       "personName" .= name'
    ,  "personAge"  .= age'
    ]

instance FromJSON Person where
  parseJSON = withObject "Expected a Person object" $ \o ->
    Person <$> o .: "name"
           <*> o .: "age"

instance ToADTArbitrary Person
instance Arbitrary Person where
  arbitrary = genericArbitrary
