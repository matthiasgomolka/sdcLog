#' Example datasets used in the vignette
#' @description Recreated after the example data from corresponding Stata
#'   function.
#' @name sdc_DT
#' @docType data
#' @usage data("sdc_DT")
#' @format A data.table with 100 rows and 8 columns.
#' @details The data.table contains the following columns:
#' - id [integer] random identifier
#' - time [integer] random time variable
#' - V1 - V3 [numeric] random variables
#' - D1 - D3 [logical] non-random dummy variables
#' @keywords datasets
"sdc_DT"

#' Example data for `sdc_descriptives()`
#' @description Utilized in the vignette.
#' @name sdc_descriptives_DT
#' @docType data
#' @usage data("sdc_descriptives_DT")
#' @format A data.table with 20 rows and 5 columns.
#' @details The data.table contains the following columns:
#' - id [factor] random identifier
#' - sector [factor] economic sector
#' - year [integer] time variable
#' - val_1, val_2 [numeric] value variables
#' @keywords datasets
"sdc_descriptives_DT"

#' Example data for `sdc_model()`
#' @description Utilized in the vignette
#' @name sdc_model_DT
#' @docType data
#' @usage data("sdc_model_DT")
#' @format A data.table with 80 rows and 9 columns.
#' @details The data.table contains the following columns:
#'   - id [factor] random identifier
#'   - y - x_4 [numeric] value variables
#'   - dummy_1 - dummy_3 [factor] dummy variables
#' @keywords datasets
"sdc_model_DT"

#' Example data for `sdc_extreme()`
#' @description Utilized in the vignette
#' @name sdc_extreme_DT
#' @docType data
#' @usage data("sdc_extreme_DT")
#' @format A data.table with 20 rows and 6 columns.
#' @details The data.table contains the following columns:
#' - id [factor] random identifier
#' - sector [factor] economic sector
#' - year [integer] time variable
#' - val_1 - val_3 [numeric] value variables
#' @keywords datasets
"sdc_extreme_DT"
