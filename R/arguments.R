#' arguments
#' @name common_arguments
#'
#' @param data [data.frame] from which the descriptive statistics are
#'   calculated.
#'
#' @param id_var [character] The name of the id variable. Defaults to `
#'   getOption("sdc.id_var")` so that you can provide `options(sdc.id_var =
#'   "my_id_var")` at the top of your script.
#'
#' @param val_var [character] vector of value variables on which descriptive
#'   statistics are computed.
#'
#' @param by [character] vector of grouping variables.
#'
#' @param zero_as_NA [logical] If TRUE, zeros in 'val_var' are treated as NA.
#'
#' @param fill_id_var [logical] Only for very specific use cases. For example:
#'
#' * `id_var` contains `NA` values which represent missing values in the sense
#' that there actually exist values identifying the entity but are unknown (or
#' deleted for privacy reasons).
#' * `id_var` contains `NA` values which result from the fact that an
#' observation features more than one  confidential identifier and not all of
#' these identifiers are present in each observation. Examples for such
#' identifiers are the role of a broker in a security transaction or the role of
#' a collateral giver in a credit relationship.
#'
#' If `TRUE`, `NA` values within `id_var` will internally be filled with
#' `<filled_[i]>`, assuming that all `NA` values of `id_var` can be treated as
#' different small entities for statistical disclosure control purposes. Thus,
#' set `TRUE` only if this is a reasonable assumption.
#'
#' Defaults to `FALSE`.
#'
#' @param model The estimated model object. Can be a model type like [lm], [glm]
#'   and various others (anything which can be handled by [broom::augment()]).
#'
#' @param min_obs [integer] The minimum number of observations used to calculate
#'   the minimum and maximum. Defaults to `getOption("sdc.n_ids", 5L)`. *This is
#'   not the number of distinct entities.*
#'
#' @param max_obs [integer] The maximum number of observations used to calculate
#'   the minimum and maximum. Defaults to `nrow(data)`. *This is not the number
#'   of distinct entities.*
#'
NULL
