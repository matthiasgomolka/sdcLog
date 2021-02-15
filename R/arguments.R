# pure technical settings
.datatable.aware <- TRUE

#' arguments
#' @name common_arguments
#' @param data [data.frame] from which the descriptive statistics are
#'   calculated.
#' @param id_var [character] The name of the id variable. Defaults to `
#'   getOption("sdc.id_var")` so that you can provide `options(sdc.id_var =
#'   "my_id_var")` at the top of your script.
#' @param val_var [character] vector of value variables on which descriptive
#'   statistics are computed.
#' @param by [character] vector of grouping variables.
#' @param zero_as_NA [logical] If TRUE, zeros in 'val_var' are treated as NA.
#' @param model The estimated model object. Can be a model type like [lm], [glm]
#'   and various others (anything which can be handled by [broom::augment()]).
#' @param min_obs [integer] The minimum number of observations used to calculate
#'   the minimum and maximum. Defaults to `getOption("sdc.n_ids", 5L)`. *This is
#'   not the number of distinct entities.*
#' @param max_obs [integer] The maximum number of observations used to calculate
#'   the minimum and maximum. Defaults to `nrow(data)`. *This is not the number
#'   of distinct entities.*
NULL
