{-|
Module      : Test.Aeson.Internal.RandomSamples
Description : Types and functions to faciliate sampling
Copyright   : (c) Plow Technologies, 2016
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : Beta

Internal module, use at your own risk.
-}

{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}

module Test.Aeson.Internal.RandomSamples where

import           Test.Aeson.Internal.Utils (aesonDecodeIO)

import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Int (Int32)

import           GHC.Generics

import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Random


-- | RandomSamples, using a seed allows you to replicate an arbitrary. By
-- storing the seed and the samples (previously produced arbitraries), we can
-- try to reproduce the same samples by generating the arbitraries with a seed.

data RandomSamples a = RandomSamples {
  seed    :: Int32
, samples :: [a]
} deriving (Eq, Ord, Show, Generic)

instance FromJSON a => FromJSON (RandomSamples a)
instance ToJSON   a => ToJSON   (RandomSamples a)

-- | Apply the seed.
setSeed :: Int -> Gen a -> Gen a
setSeed rSeed (MkGen g) = MkGen $ \ _randomSeed size -> g (mkQCGen rSeed) size

-- | Reads the seed without looking at the samples.
readSeed :: ByteString -> IO Int32
readSeed = fmap seed . aesonDecodeIO @(RandomSamples Value)

-- | Read the sample size.
readSampleSize :: ByteString -> IO Int
readSampleSize = fmap (length . samples) . aesonDecodeIO @(RandomSamples Value)
