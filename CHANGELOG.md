The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added

- Provide functions for concatenation `concatRow`, `concatLines`, and
  `concatGrid`. (#10)


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

