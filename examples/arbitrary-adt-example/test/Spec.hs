import Data.Proxy
import Lib (Person, OnOrOff)
import Test.Aeson.GenericSpecs (mkGoldenFileForType)

main :: IO ()
main = do
  mkGoldenFileForType 10 (Proxy :: Proxy Person) "golden"
  mkGoldenFileForType 10 (Proxy :: Proxy OnOrOff) "golden"
  
