# sdcLog 0.5.0

### Improvements

* SDC Results are now presented using {cli} instead of {crayon}. This made it 
  possible to improve the overall readability of the SDC results.
* All `sdc_*` functions gain the new argument `fill_id_var`. This makes output
  control easier in specific cases where you need to check an identifier with 
  many missing values. See 
  [this discussion](https://github.com/matthiasgomolka/sdcLog/issues/69) for
  details.
* For `options(sdc.info_level = 2)`, the information on dominance now prints the 
  dominance, similar to the number of distinct identifiers.
  
# sdcLog 0.4.0

### Improvements

* Introduction of [mathjaxr](https://CRAN.R-project.org/package=mathjaxr) to
  improve the quality and readability of the help files.
* [lintr](https://CRAN.R-project.org/package=lintr) induced cleanup of the 
  code base.

### Bug Fixes

* Fixed https://github.com/matthiasgomolka/sdcLog/issues/79.
* Fixed https://github.com/matthiasgomolka/sdcLog/issues/83.
* Fixed https://github.com/matthiasgomolka/sdcLog/issues/75.

# sdcLog 0.3.0

### Possibly Breaking Changes

* The objects returned by `sdc_*()` functions now contain options and settings
  in a much better structure. Before, it was a bunch of strings which could be
  pasted together. Now, it's a list holding only the relevant values. The print
  output remained almost identical (minor improvements) due to new print 
  methods.

### Improvements
* `as.data.table()` in `sdc_*()` functions is now conditional so that it's only
  called when `data` is not a `data.table` yet. This can save memory and improve
  performance.
* `sdc_model()` gained the new argument `local` (like in `source()`), which
  allows new use cases. Thanks to Pantelis Karapanagiotis for the PR!
  
### Bug Fixes
* `sdc_model()` no longer returns wrong results for `felm` models when the
  `id_var` is used for clustering.

# sdcLog 0.2.0

### Possibly Breaking Changes

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
