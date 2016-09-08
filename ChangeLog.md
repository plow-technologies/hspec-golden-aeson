# Revision history for hspec-golden-aeson

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