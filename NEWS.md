# sdcLog 0.2.0

### Breaking Changes

* The `by` argument in `sdc_descriptives()` and `sdc_extreme()` now only accepts
  character input. This makes the code more robust and easier to maintain.
* the structure of objects of type `sdc_model` is simplified (`terms` instead of
  `dummies` and `interactions`)
* `sdc_extreme()` now return the number of distinct ID's (instead of number of
  observations) used to calculate the extreme values

### Features

* support for interaction terms in `sdc_model()`
* support for checking continuous variables in `sdc_model()`
* `sdc_model()` now checks if `data` was actually used to create `model` (this
  only works if `model` has a suitable S3 method for `model.frame()`)
* the argument `id_var` now takes the default value of `getOption("sdc.id_var")`
  in all functions, which makes it possible to use `options(sdc.id_var = "id")` 
  at the top of a script and save some typing
* performance improvement for `check_dominance()` (and therefore in
  `sdc_descriptives()` and `sdc_extreme()`)

### Bug Fixes

* `check_dominance()` now handles negative values correctly (as `abs()`)


# sdcLog 0.1.0 (first CRAN release)

* Added a `NEWS.md` file to track changes to the package.
