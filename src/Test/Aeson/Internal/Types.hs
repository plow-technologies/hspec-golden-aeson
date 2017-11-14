{-|
Module      : Test.Aeson.Internal.Type
Description : Golden tests for Arbitrary
Copyright   : (c) Plow Technologies, 2016
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : Beta

Internal module, use at your own risk.
-}

{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Aeson.Internal.Types where

import           Data.Proxy
import           Data.Typeable
import           Test.Aeson.Internal.Utils
import           Test.QuickCheck

--------------------------------------------------
-- Handle creating names
--------------------------------------------------

newtype TopDir =
  TopDir
    { unTopDir :: FilePath
    } deriving (Eq,Read,Show)

newtype ModuleName =
  ModuleName
    { unModuleName :: FilePath
    } deriving (Eq,Read,Show)

newtype TypeName =
  TypeName
    { unTypeName :: FilePath
    } deriving (Eq,Read,Show)

data TypeNameInfo a =
  TypeNameInfo
    { typeNameTopDir :: TopDir
    , typeNameModuleName :: Maybe ModuleName
    , typeNameTypeName   :: TypeName
    } deriving (Eq,Read,Show)

mkTypeNameInfo :: forall a . Arbitrary a => Typeable a => Settings -> Proxy a -> IO (TypeNameInfo a)
mkTypeNameInfo (Settings { useModuleNameAsSubDirectory
                       , goldenDirectoryOption}) proxy = do
  maybeModuleName <- maybeModuleNameIO
  return $ TypeNameInfo (TopDir         topDir )
                        (ModuleName <$> maybeModuleName )
                        (TypeName typeName)
  where
   typeName = show (typeRep proxy)
   maybeModuleNameIO =
     if useModuleNameAsSubDirectory
     then do
       arbA <- generate (arbitrary :: Gen a)
       return $ Just $ tyConModule . typeRepTyCon . typeOf $ arbA
     else return Nothing

   topDir =
     case goldenDirectoryOption of
       GoldenDirectory -> "golden"
       CustomDirectoryName d -> d
