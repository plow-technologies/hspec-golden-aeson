module Main where

import           Test.Hspec (hspec)
import qualified Test.Aeson.GenericSpecs.Legacy as Legacy

main :: IO ()
main = do
  hspec Legacy.spec
