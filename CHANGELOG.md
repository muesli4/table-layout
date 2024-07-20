The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

## [1.0.0.1] - 2024-07-20

### Changed

- Changed version bounds of `base` and `QuickCheck`.

## [1.0.0.0] - 2023-11-17

This release would not have been possible without the tireless work of Stephen
Morgan.

### Added

- Add `asciiDoubleS` table style.
- Add combinators for removing outer borders of tables.
- Provide general versions of grid and table functions that generate output for
  `StringBuilder` instances.
- Add dependency to `doclayout` and `text`.
- Add `WideString` and `WideText` to support multi-width character input. (#8)
- Custom vertical separators can now be specified in the table header
  specification in a hierarchic manner.
- Add simplified table style specification with `LineStyle` and many new
  combinators.
- Add `Cell` instance for `Data.Text`, `Data.Either`, `Data.Maybe`.
- Add versions of functions for tables and grids that return `ColModInfo`s.
- Provide functions to work with `Formatted`.
- Add safe versions of the `trim` function. (#35)
- Add `expandBetween` `LenSpec` which provides a lower and an upper bound for
  the width of a column. (#35)
- Add explicit defaults for data types (#26).
- Provide more functions to derive `ColModInfo`.
- Add `TableSpec` to specify tables.

### Changed

- Changed version bounds of `base`, `QuickCheck`, and `data-default-class`.
- Remove dependency on `data-default-instances-base`.
- Improve performance of Cell and StringBuilder instances
- Generalize `Formatted` to provide nested color formatting. (#11)
- Change `TableStyle` to use `String` as basic building blocks of tables. (#17)
- Add more separator types to `TableStyle`.
- Accept arbitrary instances of `Cell` as header titles.
- Lower requirements for `Cell` instances:  Cells no longer need to be able to
  drop characters.
- Drop `Monoid` requirement for vertical alignment.
- Rendering functions for tables now use `TableSpec`.
- Renamed functions to derive `ColModInfo`.


### Fixed

- Fix recently introduced errors which made text justification completely
  unusable.  Add test coverage for relevant functions. (#15)

## [0.9.0.2] - 2020-12-20

### Fixed

- Fix error in `fitWords` that was reversing the word order.  This also affects
  `justify` and `justifyWords`. (#14)

## [0.9.0.1] - 2020-06-14

### Added

- Provide functions for concatenation `concatRow`, `concatLines`, and
  `concatGrid`. (#10)

### Fixed

- Fix an error with `applyCutInfo` that created a string of wrong length in
  case the cut mark was of unequal length and the view was outside of the cell.


## [0.9.0.0] - 2020-04-10

### Added

- Add `Text.Layout.Table.Cell.Formatted`. A `Cell` instance is provided that
  enables formatting text correctly with inline text formatting commands.
- Add helpers to generate tables for Pandoc in the module
  `Text.Layout.Table.Pandoc`.
- Add `Cell` type class: This enables using different input string types.
- Add `StringBuilder` type class. This enables generating output in different
  ways.
- Add test case for left-biased trim.

### Changed

- Change input type of a lot of functions to use the `Cell` type class. This
  might break some code but will require only a type annotation to fix.
- Rework and simplify formatting functions with `Cell` and `StringBuilder`. This
  includes improvements in test coverage.
- Reorganize module structure:
    * Modules of types used for specification do now start with
      `Text.Layout.Table.Spec.`.
    * Move a lot of the code in `Text.Layout.Table` into sub-modules.

### Fixed

- Fix an error with text justification for lines that contain only one word and
  add a test case. (#6)
- Fix an error where cut marks where applied wrongly on the right side.

