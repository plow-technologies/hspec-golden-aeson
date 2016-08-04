{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Test.Aeson.Internal.RandomSamples where

import           Control.Exception
import           Control.Monad

import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy

import           GHC.Generics

import           System.Random

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.ADT
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Random


-- | RandomSamples, using a seed allows you to replicate an arbitrary. By
-- storing the seed and the samples (previously produced arbitraries), we can
-- try to reproduce the same samples by generating the arbitraries with a seed
--

data RandomSamples a = RandomSamples {
  seed    :: Int
, samples :: [a]
} deriving (Eq, Ord, Show, Generic)

instance FromJSON a => FromJSON (RandomSamples a)
instance ToJSON   a => ToJSON   (RandomSamples a)

setSeed :: Int -> Gen a -> Gen a
setSeed seed (MkGen g) = MkGen $ \ _randomSeed size -> g (mkQCGen seed) size

-- reads the seed without looking at the samples
readSeed :: ByteString -> IO Int
readSeed s = case eitherDecode s :: Either String (RandomSamples Value) of
  Right samples -> return $ seed samples
  Left err -> throwIO $ ErrorCall err
