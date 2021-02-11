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
#' @param model The estimated model object. Can be a model type like lm, glm and
#'   various others (anything which can be handled by [broom::augment()]).
#' @param n_min [integer] The number of values used to calculate the minimum, by
#'   default 5.
#' @param n_max [integer] The number of values used to calculate the maximum, by
#'   default 5.
NULL
