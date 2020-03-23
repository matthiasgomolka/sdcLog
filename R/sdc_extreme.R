#' check if calculation of extreme values comply to RDSC rules.
#'   If so, function returns average min and max values according to RDSC rules.
#' @param data [data.frame] The dataset (anything which can be coerced to data.table) from
#'   which the extreme values can be calculated.
#' @param id_var [character] The name of the id variable as a character.
#' @param val_var [character] Character vector of value variables on which descriptives are
#'   computed.
#' @param by Grouping variables. Can be provided as in
#'   [data.table::data.table()].
#' @param n_min [integer] The number of values used to calculate the minimum, by default 5.
#' @param n_max [integer] The number of values used to calculate the maximum, by default 5.
#' @importFrom data.table as.data.table data.table setorderv fintersect .N
#' @importFrom checkmate assert_int
#' @export

sdc_extreme <- function(
  data,
  id_var,
  val_var,
  by = NULL,
  n_min = 5L,
  n_max = n_min
) {
  # input checks
  check_args(data, id_var, val_var, by)
  checkmate::assert_int(n_max)
  checkmate::assert_int(n_min)

  # status messages
  message_options()
  message_arguments(id_var, val_var, by)

  data <- data.table::as.data.table(data)

  # na.omit.data.table call
  data <- na.omit(data, cols = val_var)

  # order decreasing by val_var
  data.table::setorderv(data, cols = val_var, order = -1L)

  # find SD's for min and max
  results_min <- find_SD(data, "min", n_min, id_var, val_var, by)
  results_max <- find_SD(data, "max", n_max, id_var, val_var, by)

  # check for overlaps of results
  sd_overlap <- nrow(data.table::fintersect(results_min, results_max)) > 0
  if (!sd_overlap) {
      if (!is.null(by)) {
          data.table::data.table(
              val_var,
              results_min[, list(min = mean(get(val_var))), by = by],
              results_min[, list(n_obs_min = .N), by = by][, 2],
              results_max[, list(max = mean(get(val_var))), by = by][, 2],
              results_max[, list(n_obs_max = .N), by = by][, 2])
      } else {
          data.table::data.table(
              val_var = val_var,
              min = mean(results_min[[val_var]]),
              n_obs_min = nrow(results_min),
              max = mean(results_max[[val_var]]),
              n_obs_max = nrow(results_max)
          )
      }
  } else {
    message("It is impossible to compute extreme values for variable '",
            val_var, "' that comply to RDSC rules.")
      if(!is.null(by)){
        data.table::data.table(
            val_Var = val_var,
            by = unique(data[, get(by)]),
            min = rep(NA_real_, length(unique(data[, get(by)]))),
            n_obs_min = rep(NA_integer_, length(unique(data[, get(by)]))),
            max = rep(NA_real_, length(unique(data[, get(by)]))),
            n_obs_max = rep(NA_integer_, length(unique(data[, get(by)])))
        )
        } else {
        data.table::data.table(
            val_var = val_var,
            min = NA_real_,
            n_obs_min = NA_integer_,
            max = NA_real_,
            n_obs_max = NA_integer_
            )
    }
  }
}

#' @importFrom utils tail head
find_SD <- function(data, type, n, id_var, val_var, by) {
  SD_fun <- switch(type,
                   min = utils::tail,
                   max = utils::head
  )

  SD_results <- find_SD_problems(data, SD_fun, n, id_var, val_var, by)

  while (SD_results[["problems"]]) {
    n <- n + 1
    SD_results <- find_SD_problems(data, SD_fun, n, id_var, val_var, by)

    # this assures that this is no infinite loop; problems will be catched
    # during the check for overlaps
    if (n == nrow(data)) {
      return(SD_results[["SD"]])
    }
  }

  return(SD_results[["SD"]])
}

#' @importFrom data.table .SD
find_SD_problems <- function(data, SD_fun, n, id_var, val_var, by) {
  SD <- data[order(-get(val_var)), SD_fun(.SD, n), by = by]

  results_distinct_ids <- eval(eval(substitute(
    check_distinct_ids(SD, id_var, val_var, by),
    env = parent.frame(n = 2L)
    )))
  class(results_distinct_ids) <- c("sdc_counts", class(results_distinct_ids))

  results_dominance <- eval(eval(substitute(
    check_dominance(SD, id_var, val_var, by),
    env = parent.frame(n = 2L)
  )))
  class(results_dominance) <- c("sdc_dominance", class(results_dominance))

  list(
    SD = SD,
    problems = sum(nrow(results_distinct_ids), nrow(results_dominance)) != 0L
  )
}

