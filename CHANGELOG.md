# Revision history for named-text

## 1.1.4.0 -- 2023-10-01

* Update to allow Sayable 1.2.0.0

## 1.1.3.0 -- 2023-06-20

* Allow build with GHC 9.6.

## 1.1.2.0 -- 2023-01-17

* Updated sayable package upper bound to < 1.2.

## 1.1.1.0 -- 2022-12-28

* Added optional Data.Name.JSON providing `JSONSchema` with Aeson `ToJSON` and
  `FromJSON` instances.
* Additional tests.
* Added README and updated nix flake.

## 1.1.0.0 -- 2022-12-28

* Added tests and enhanced haddock.
* Re-organized implementation, removing extraneous definitions.
* The `name` and `caselessName` functions are deprecated in favor of `nameText`.
* General, overlappable `Prettyprinter` `Pretty` instance for all `Named`.
* Changed from `Named style sym` to `Named style nameOf`.
* Fixed `convertStyle` to use the proper `fromText` instance.
* Added `NameText` constraint to `viewSomeNameStyle` first argument signature.
* Added `nameLength` and `nullName` utility functions.

## 1.0.1.0 -- 2022-12-23

* Specific GHC support range for GHC 8.8--9.4
* Small cabal file adjustments.

## 1.0.0.0 -- 2022-06-29

* First independent version.
