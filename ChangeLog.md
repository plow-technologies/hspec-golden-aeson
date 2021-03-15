# Revision history for hspec-golden-aeson

## 0.9.0.0 -- 2021-03-15
* Breaking change: Objects are now serialized with sorted keys for better
  cross-platform compatibility

## 0.8.0.0 -- 2021-03-12

* Breaking change: Seed is now an `Int32` so golden files are more portable. This
  requires regenerating all golden files which have a seed that overflows
* Breaking change: Golden files are no longer generated automatically if they
  don't exist, to create them, set the `CREATE_MISSING_GOLDEN` environment variable.
  This is to prevent missing golden files from silently making golden tests
  degrade to round-trip tests
* Add a `RECREATE_BROKEN_GOLDEN` environemnt variable. When present it will
  cause golden files to be re-created if they cause the test to fail. This is
  useful for updating golden files when serialization has been purposedly
  modified and to update the seed if it breaks due to overflow now that it is
  only 32bit wide.

## 0.7.0.0 -- 2018-05-17

* Breaking change: allow roundtripAndGoldenADTSpecs test to pass when random samples generated from the seed in the golden file do not produce the same Haskell samples, but yet decoding and re-encoding the golden file still produces the same bytes as in the golden file.
* Add an additional faulty file ending in `.faulty.reencoded.json` when the byte-for-byte decode/encode round-trip fails. This allows you to compare the encoding changes without the noise of the random sample change. In this case, the test will output a message indicating whether decoding the golden file produces the same Haskell values as decoding the re-encoded files. If they produce the same values, that is likely a minor encoding change, but still a change so tests fail.
* Add `RandomMismatchOption` to `Settings` so you can have the old behavior of failing tests when random samples change.

## 0.6.0.0 -- 2018-01-04

* Test encoding in `roundtripAndGoldenADTSpecs' and 'roundtripAndGoldenADTSpecsWithSettings` functions. This may break current tests because only decoding was tested previously.

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
