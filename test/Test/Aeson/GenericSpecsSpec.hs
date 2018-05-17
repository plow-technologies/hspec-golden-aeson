{-# LANGUAGE OverloadedStrings #-}

module Test.Aeson.GenericSpecsSpec where

import           Data.Proxy

import           System.Directory

import           Test.Aeson.GenericSpecs
import           Test.Aeson.Internal.Utils (RandomMismatchOption(..))
import           Test.Hspec
import           Test.Hspec.Core.Runner

import           Test.Utils

-- various iterations of a Product and Sum Type and their serializations
import qualified Test.Types                     as T
import qualified Test.Types.AlteredSelector     as TAS
import qualified Test.Types.BrokenSerialization as TBS
import qualified Test.Types.MismatchedToAndFromSerialization as MTFS
import qualified Test.Types.NewSelector         as TNS


-- summaryFailures
spec :: Spec
spec = do
  describe "Test.Aeson.GenericSpecs: roundtripSpecs" $ do
    it "should pass when ToJSON and FromJSON are defined appropriately" $ do
      (s1,_) <- hspecSilently $ roundtripSpecs (Proxy :: Proxy T.Person)
      summaryFailures s1 `shouldBe` 0

    it "should fail when ToJSON and FromJSON definitions do not match" $ do
      (s1,_) <- hspecSilently $ roundtripSpecs (Proxy :: Proxy MTFS.Person)
      summaryFailures s1 `shouldBe` 1

  describe "Test.Aeson.GenericSpecs: roundtripADTSpecs" $ do
    it "should pass when ToJSON and FromJSON are defined appropriately" $ do
      (s1,_) <- hspecSilently $ roundtripADTSpecs (Proxy :: Proxy T.Person)
      summaryFailures s1 `shouldBe` 0

    it "should fail when ToJSON and FromJSON definitions do not match" $ do
      (s1,_) <- hspecSilently $ roundtripADTSpecs (Proxy :: Proxy MTFS.Person)
      summaryFailures s1 `shouldBe` 1

  describe "Test.Aeson.GenericSpecs: goldenSpecs" $ do
    it "create golden test files" $ do
      -- clean up previously existing golden folder
      bg <- doesDirectoryExist "golden"
      if bg
        then removeDirectoryRecursive "golden"
        else return ()

      -- files for Person and SumType do not exist
      -- create them by running goldenADTSpecs
      _ <- hspecSilently $ goldenSpecs defaultSettings (Proxy :: Proxy T.Person)
      _ <- hspecSilently $ goldenSpecs defaultSettings (Proxy :: Proxy T.SumType)

      doesFileExist "golden/Person.json"  `shouldReturn` True
      doesFileExist "golden/SumType.json" `shouldReturn` True

    it "create golden test files" $ do
      -- clean up previously existing golden folder
      bg <- doesDirectoryExist "golden"
      if bg
        then removeDirectoryRecursive "golden"
        else return ()

      -- files for Person and SumType do not exist
      -- create them by running goldenADTSpecs
      _ <- hspecSilently $ goldenSpecs (defaultSettings { useModuleNameAsSubDirectory = True }) (Proxy :: Proxy T.Person)
      _ <- hspecSilently $ goldenSpecs (defaultSettings { useModuleNameAsSubDirectory = True }) (Proxy :: Proxy T.SumType)

      doesFileExist "golden/Test.Types/Person.json"  `shouldReturn` True
      doesFileExist "golden/Test.Types/SumType.json" `shouldReturn` True

    it "create golden test files in a user defined directory" $ do
      let topDir = "json-tests"
      -- clean up previously existing user defined folder
      bg <- doesDirectoryExist topDir
      if bg
        then removeDirectoryRecursive topDir
        else return ()

      -- files for Person and SumType do not exist
      -- create them by running goldenADTSpecs
      _ <- hspecSilently $ goldenSpecs (defaultSettings {goldenDirectoryOption = CustomDirectoryName topDir}) (Proxy :: Proxy T.Person)
      _ <- hspecSilently $ goldenSpecs (defaultSettings {goldenDirectoryOption = CustomDirectoryName topDir}) (Proxy :: Proxy T.SumType)

      doesFileExist "json-tests/Person.json"  `shouldReturn` True
      doesFileExist "json-tests/SumType.json" `shouldReturn` True

    it "goldenADTSpecs should pass for existing golden files in which model types and serialization have not changed" $ do
      (s1,_) <- hspecSilently $ goldenSpecs defaultSettings (Proxy :: Proxy T.Person)
      (s2,_) <- hspecSilently $ goldenSpecs defaultSettings (Proxy :: Proxy T.SumType)
      (summaryFailures s1 + summaryFailures s2) `shouldBe` 0

    it "goldenADTSpecs for types which have changed the values of ToJSON or FromJSON keys should fail to match the goldenFiles" $ do
      (s1,_) <- hspecSilently $ goldenSpecs defaultSettings (Proxy :: Proxy TBS.Person)
      summaryFailures s1 `shouldBe` 1

    it "goldenADTSpecs for types which have changed the values of ToJSON or FromJSON keys should fail to match the goldenFiles" $ do
      (s1,_) <- hspecSilently $ goldenSpecs defaultSettings (Proxy :: Proxy TNS.Person)
      summaryFailures s1 `shouldBe` 1

    it "goldenADTSpecs for types which have altered the name of the selector and using generic implementation of ToJSON and FromJSON should fail to match the goldenFiles" $ do
      (s1,_) <- hspecSilently $ goldenSpecs defaultSettings (Proxy :: Proxy TAS.Person)
      summaryFailures s1 `shouldBe` 1


  describe "Test.Aeson.GenericSpecs: goldenADTSpecs" $ do
    it "create golden test files" $ do
      -- clean up previously existing golden folder
      bg <- doesDirectoryExist "golden"
      if bg
        then removeDirectoryRecursive "golden"
        else return ()

      -- files for Person and SumType do not exist
      -- create them by running goldenADTSpecs
      _ <- hspecSilently $ goldenADTSpecs defaultSettings (Proxy :: Proxy T.Person)
      _ <- hspecSilently $ goldenADTSpecs defaultSettings (Proxy :: Proxy T.SumType)

      doesFileExist "golden/Person/Person.json"    `shouldReturn` True
      doesFileExist "golden/SumType/SumType1.json" `shouldReturn` True
      doesFileExist "golden/SumType/SumType2.json" `shouldReturn` True
      doesFileExist "golden/SumType/SumType3.json" `shouldReturn` True


    it "create golden test files in a sub directory using the module name" $ do
      -- files for Person and SumType do not exist
      -- create them by running goldenADTSpecs
      _ <- hspecSilently $ goldenADTSpecs defaultSettings {useModuleNameAsSubDirectory = True } (Proxy :: Proxy T.Person)
      _ <- hspecSilently $ goldenADTSpecs defaultSettings {useModuleNameAsSubDirectory = True } (Proxy :: Proxy T.SumType)

      doesFileExist "golden/Test.Types/Person/Person.json"    `shouldReturn` True
      doesFileExist "golden/Test.Types/SumType/SumType1.json" `shouldReturn` True
      doesFileExist "golden/Test.Types/SumType/SumType2.json" `shouldReturn` True
      doesFileExist "golden/Test.Types/SumType/SumType3.json" `shouldReturn` True

    it "create golden test files in a user defined directory" $ do
      let topDir = "json-tests"
      -- clean up previously existing user defined folder
      bg <- doesDirectoryExist topDir
      if bg
        then removeDirectoryRecursive topDir
        else return ()

      -- files for Person and SumType do not exist
      -- create them by running goldenADTSpecs
      _ <- hspecSilently $ goldenADTSpecs (defaultSettings {goldenDirectoryOption = CustomDirectoryName topDir}) (Proxy :: Proxy T.Person)
      _ <- hspecSilently $ goldenADTSpecs (defaultSettings {goldenDirectoryOption = CustomDirectoryName topDir}) (Proxy :: Proxy T.SumType)

      doesFileExist "json-tests/Person/Person.json"    `shouldReturn` True
      doesFileExist "json-tests/SumType/SumType1.json" `shouldReturn` True
      doesFileExist "json-tests/SumType/SumType2.json" `shouldReturn` True
      doesFileExist "json-tests/SumType/SumType3.json" `shouldReturn` True

    it "goldenADTSpecs should pass for existing golden files in which model types and serialization have not changed" $ do
      (s1,_) <- hspecSilently $ goldenADTSpecs defaultSettings (Proxy :: Proxy T.Person)
      (s2,_) <- hspecSilently $ goldenADTSpecs defaultSettings (Proxy :: Proxy T.SumType)
      (summaryFailures s1 + summaryFailures s2) `shouldBe` 0

    it "goldenADTSpecs for types which have changed the values of ToJSON or FromJSON keys should fail to match the goldenFiles" $ do
      (s1,_) <- hspecSilently $ goldenADTSpecs defaultSettings (Proxy :: Proxy TBS.Person)
      summaryFailures s1 `shouldBe` 1

    it "goldenADTSpecs for types which have changed the values of ToJSON or FromJSON keys should fail to match the goldenFiles" $ do
      (s1,_) <- hspecSilently $ goldenADTSpecs defaultSettings (Proxy :: Proxy TNS.Person)
      summaryFailures s1 `shouldBe` 1

    it "goldenADTSpecs for types which have altered the name of the selector and using generic implementation of ToJSON and FromJSON should fail to match the goldenFiles" $ do
      (s1,_) <- hspecSilently $ goldenADTSpecs defaultSettings (Proxy :: Proxy TAS.Person)
      summaryFailures s1 `shouldBe` 1

    -- golden file which appends `.0` to all ages.
    -- write as inline text to make test case clearer
    let
      goldenReadCompatible = "{\
\    \"seed\": -558805430132139320,\
\    \"samples\": [\
\        {\
\            \"age\": -24.0,\
\            \"name\": \"<\147\158\\u000e\251\133]V_t\167\&2\210\148\194Q\196_\150\145\\u0017Hf&\"\
\        },\
\        {\
\            \"age\": -9.0,\
\            \"name\": \"<\\n\\u0018O\\n>\"\
\        },\
\        {\
\            \"age\": -1.0,\
\            \"name\": \"m0x\217\\u000c\\u000fg4\188\197\\u000bF\227\\u0013\\u00042F\\u001f\176%`\224Iw\\u0011\231/\204\"\
\        },\
\        {\
\            \"age\": -22.0,\
\            \"name\": \"x\221\250{\\u001bG\244\\u0012\232\\u0004)\"\
\        },\
\        {\
\            \"age\": 26.0,\
\            \"name\": \"\153\\u0005n\\u001a$q27\184=\\u0012ba\\u001b\\u0010\252]\242A.\139w\234\&0\239\\u0004\204\"\
\        }\
\    ]\
\}"

    it "encoding read-compatible change should fail test" $ do
      -- clean up previously existing golden folder
      bg <- doesDirectoryExist "golden"
      if bg
        then removeDirectoryRecursive "golden"
        else return ()

      -- directly create golden file for tests
      createDirectoryIfMissing True "golden/Person"
      writeFile "golden/Person/Person.json" goldenReadCompatible

      -- Aeson can decode "10.0" as `10 :: Int` but it will encode `10 :: Int` as 10
      -- This should cause the golden test to fail since it indicates a change in encoding
      (s1,_) <- hspecSilently $ goldenADTSpecs defaultSettings (Proxy :: Proxy T.Person)
      summaryFailures s1 `shouldBe` 1

    let
      goldenByteIdentical = "{\n\
\    \"seed\": 41,\n\
\    \"samples\": [\n\
\        {\n\
\            \"age\": 1,\n\
\            \"name\": \"abc\"\n\
\        },\n\
\        {\n\
\            \"age\": 2,\n\
\            \"name\": \"def\"\n\
\        }\n\
\    ]\n\
\}"

    it "different random seed but byte-for-byte identical should pass (default setting)" $ do
      -- clean up previously existing golden folder
      bg <- doesDirectoryExist "golden"
      if bg
        then removeDirectoryRecursive "golden"
        else return ()

      -- directly create golden file for tests
      createDirectoryIfMissing True "golden/Person"
      writeFile "golden/Person/Person.json" goldenByteIdentical

      (s1,_) <- hspecSilently $ goldenADTSpecs defaultSettings (Proxy :: Proxy T.Person)
      summaryFailures s1 `shouldBe` 0

    it "different random seed but byte-for-byte identical should fail (with custom setting)" $ do
      -- clean up previously existing golden folder
      bg <- doesDirectoryExist "golden"
      if bg
        then removeDirectoryRecursive "golden"
        else return ()

      -- directly create golden file for tests
      createDirectoryIfMissing True "golden/Person"
      writeFile "golden/Person/Person.json" goldenByteIdentical

      let
        customSettings = defaultSettings { randomMismatchOption = RandomMismatchError }
      (s1,_) <- hspecSilently $ goldenADTSpecs customSettings (Proxy :: Proxy T.Person)
      summaryFailures s1 `shouldBe` 1

  describe "mkGoldenFileForType" $ do
    it "create a single file in a dir for a Product type" $ do
      -- clean up previously existing golden folder
      bg <- doesDirectoryExist "golden"
      if bg
        then removeDirectoryRecursive "golden"
        else pure ()

      mkGoldenFileForType 10 (Proxy :: Proxy T.Person) "golden"
      doesFileExist "golden/Person/Person.json"    `shouldReturn` True

    it "create a file for each constructor in a dir for a Sum type" $ do
      -- clean up previously existing golden folder
      bg <- doesDirectoryExist "golden"
      if bg
        then removeDirectoryRecursive "golden"
        else pure ()

      mkGoldenFileForType 10 (Proxy :: Proxy T.SumType) "golden"
      doesFileExist "golden/SumType/SumType1.json" `shouldReturn` True
      doesFileExist "golden/SumType/SumType2.json" `shouldReturn` True
      doesFileExist "golden/SumType/SumType3.json" `shouldReturn` True



main :: IO ()
main = hspec spec
