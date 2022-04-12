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

#' Example data for `sdc_min_max()`
#' @description Utilized in the vignette
#' @name sdc_min_max_DT
#' @docType data
#' @usage data("sdc_min_max_DT")
#' @format A data.table with 20 rows and 6 columns.
#' @details The data.table contains the following columns:
#' - id [factor] random identifier
#' - sector [factor] economic sector
#' - year [integer] time variable
#' - val_1 - val_3 [numeric] value variables
#' @keywords datasets
"sdc_min_max_DT"

#' Example data for `sdc_descriptives()`
#' @description Utilized in the unit tests
#' @name sdc_dups_DT
#' @docType data
#' @usage data("sdc_dups_DT")
#' @format A data.table with 9 rows and 5 columns.
#' @details The data.table contains the following columns:
#' - lender_group [factor] random lender group identifier
#' - lender [factor] random lender identifier
#' - volume [integer] credit volume
#' - borrower [factor] random borrower identifier
#' - borrower_group [factor] random borrower group identifier
#' @keywords datasets
"sdc_dups_DT"

#' Example data for `sdc_descriptives()`
#' @description Utilized in the unit tests
#' @name sdc_dups_credits_DT
#' @docType data
#' @usage data("sdc_dups_credits_DT")
#' @format A data.table with 6 rows and 3 columns.
#' @details The data.table contains the following columns:
#' - lender [factor] random lender identifier
#' - borrower [factor] random borrower identifier
#' - volume [integer] credit volume
#' @keywords datasets
"sdc_dups_credits_DT"

#' Example data for `sdc_descriptives()`
#' @description Utilized in the unit tests
#' @name sdc_dups_lender_groups_DT
#' @docType data
#' @usage data("sdc_dups_lender_groups_DT")
#' @format A data.table with 7 rows and 2 columns.
#' @details The data.table contains the following columns:
#' - lender_group [factor] random lender group identifier
#' - lender [factor] random lender identifier
#' @keywords datasets
"sdc_dups_lender_groups_DT"
