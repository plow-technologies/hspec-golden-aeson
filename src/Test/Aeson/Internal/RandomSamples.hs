{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Test.Aeson.Internal.RandomSamples where

import           Control.Exception

import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)

import           GHC.Generics

import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Random


-- | RandomSamples, using a seed allows you to replicate an arbitrary. By
-- storing the seed and the samples (previously produced arbitraries), we can
-- try to reproduce the same samples by generating the arbitraries with a seed

data RandomSamples a = RandomSamples {
  seed    :: Int
, samples :: [a]
} deriving (Eq, Ord, Show, Generic)

instance FromJSON a => FromJSON (RandomSamples a)
instance ToJSON   a => ToJSON   (RandomSamples a)

setSeed :: Int -> Gen a -> Gen a
setSeed rSeed (MkGen g) = MkGen $ \ _randomSeed size -> g (mkQCGen rSeed) size

-- | reads the seed without looking at the samples
readSeed :: ByteString -> IO Int
readSeed s = case eitherDecode s :: Either String (RandomSamples Value) of
  Right rSamples -> return $ seed rSamples
  Left err -> throwIO $ ErrorCall err

-- | read the sample size
readSampleSize :: ByteString -> IO Int
readSampleSize s = case eitherDecode s :: Either String (RandomSamples Value) of
  Right rSamples -> return . length . samples $ rSamples
  Left err -> throwIO $ ErrorCall err
