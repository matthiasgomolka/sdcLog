# sdcLog 0.2.0

### Breaking Changes

* The `by` argument in `sdc_descriptives()` and `sdc_extreme()` now only accepts
character input. This makes the code more robust and easier to maintain.

### Features

* support for interaction terms added to `sdc_model()`
* performance improvement for `check_dominance()` (and therefore in
`sdc_descriptives()` and `sdc_extreme()`)

### Bug Fixes

* `check_dominance()` now handles negative values correctly (as `abs()`)


# sdcLog 0.1.0 (first CRAN release)

* Added a `NEWS.md` file to track changes to the package.
