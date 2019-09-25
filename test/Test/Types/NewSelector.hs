{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Test.Types.NewSelector where

import           Data.Aeson
import           Data.Data (Data)
import           GHC.Generics
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.ADT

data Person =
  Person
    { name    :: String
    , age     :: Int
    , address :: String
    } deriving (Data,Eq,Show,Generic)

instance ToJSON Person
instance FromJSON Person
instance ToADTArbitrary Person
instance Arbitrary Person where
  arbitrary = genericArbitrary

data SumType
  = SumType1 Int
  | SumType2 String Int
  | SumType3 Double String Int
  deriving (Data,Eq,Show,Generic)

instance ToJSON SumType
instance FromJSON SumType
instance ToADTArbitrary SumType
instance Arbitrary SumType where
  arbitrary = genericArbitrary
