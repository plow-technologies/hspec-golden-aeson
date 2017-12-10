{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( Person (..)
    , OnOrOff (..)
    ) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..), oneof)
import Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary)

data Person =
  Person
    { name :: String
    , address :: String
    , age :: Int
    } deriving (Eq,Read,Show,Generic)

instance ToJSON Person
instance ToADTArbitrary Person
instance Arbitrary Person where
  arbitrary = Person <$> arbitrary <*> arbitrary <*> arbitrary

data OnOrOff
  = On
  | Off
  deriving (Eq,Read,Show,Generic)

instance ToJSON OnOrOff
instance ToADTArbitrary OnOrOff
instance Arbitrary OnOrOff where
  arbitrary = oneof [pure On, pure Off]

