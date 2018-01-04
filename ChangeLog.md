# Revision history for hspec-golden-aeson

## 0.5.1.0 -- 2018-01-04

* Remove 'Wredundant-constraints' flag.

## 0.5.0.0 -- 2018-01-02

* Add 'Arbitrary' requirement for 'roundtripADTSpecs', 'roundtripAndGoldenADTSpecs' and 'roundtripAndGoldenADTSpecsWithSettings' because 'Arbitrary' was a redundant constrain for 'ToADTArbitrary' in quickcheck-arbitrary-adt.

## 0.4.0.0 -- 2017-12-10

* Fix behavior for 'mkGoldenFileForType'. Intention is to create a file in a dir for each constructor, but it was only creating a file for one of the constructors of a type.

## 0.3.1.0 -- 2017-12-02

* Expose 'roundtripAndGoldenSpecsWithSettings'.

## 0.3.0.0  -- 2017-11-14

* Add mkGoldenFileForType.
* Rename internal function fromTypeable to mkTypeNameInfo.
* Move TopDir, ModuleName, TypeName, TypeNameInfo and mkTypeNameInfo into Test.Aeson.Internal.Util.

## 0.2.1.0  -- 2017-08-08

* Added the ability to run an automated test withought needing a Show, Eq, or Typeable instance.
* Cleaned up error messages, mostly involving redundant types

## 0.2.0.3  -- 2016-09-08

* Tests were breaking because Test.Types.MismatchedToAndFromSerialization was
missing from the cabal file.

## 0.2.0.2  -- 2016-09-07

* Forgot to add fixes to the test, in previous version they were not compiling.

## 0.2.0.1  -- 2016-09-01

* Fix error in 'goldenSpecsWithNote', behavior for useModuleNameAsSubDirectory was flipped around and also providing the module name surrounded by quotes.
* Add more tests.

## 0.2.0.0  -- 2016-08-23

* Make directory and golden file naming flexible.
* Include optional Settings type that allows users to set the directory name and size of tests.
* Require quickcheck-arbitrary-adt >= 0.2.0.0.

## 0.1.0.0  -- 2016-08-12

* First version.
