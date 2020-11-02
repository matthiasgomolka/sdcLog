#' check if calculation of extreme values comply to RDC rules.
#'   If so, function returns average min and max values according to RDC rules.
#' @param data [data.frame] The dataset from which the extreme values can be
#'   calculated.
#' @param id_var [character] The name of the id variable as a character.
#' @param val_var [character] Character vector of value variables on which
#'   descriptives are computed.
#' @param by Grouping variables. Can be provided as in
#'   [data.table::data.table()].
#' @param n_min [integer] The number of values used to calculate the minimum, by
#'   default 5.
#' @param n_max [integer] The number of values used to calculate the maximum, by
#'   default 5.
#' @importFrom data.table as.data.table data.table setorderv fintersect .N set
#' @importFrom checkmate assert_int
#' @export
#' @examples
#' sdc_extreme(data = sdc_extreme_DT, id = "id", val_var = "val_1")
#' sdc_extreme(data = sdc_extreme_DT, id = "id", val_var = "val_2")
#' sdc_extreme(data = sdc_extreme_DT, id = "id", val_var = "val_2", n_min = 7)
#' sdc_extreme(data = sdc_extreme_DT, id = "id", val_var = "val_3", n_min = 10, n_max = 10)
#' sdc_extreme(data = sdc_extreme_DT, id = "id", val_var = "val_3", n_min = 8, n_max = 8)
#' sdc_extreme(data = sdc_extreme_DT, id_var = "id", val_var = "val_1", by = year)
#' sdc_extreme(data = sdc_extreme_DT, id_var = "id", val_var = "val_1", by = c("sector", "year"))
#' @return A list [list] with detailed information about options, settings and the calculated extreme values (if possible).


sdc_extreme <- function(
                        data,
                        id_var,
                        val_var,
                        by = NULL,
                        n_min = 5L,
                        n_max = n_min) {
  # input checks
  check_args(data, id_var, val_var, by)
  checkmate::assert_int(n_max)
  checkmate::assert_int(n_min)

  # status messages
  message(message_options())
  message(message_arguments(id_var, val_var, by))

  data <- data.table::as.data.table(data)

  # na.omit.data.table call
  data <- na.omit(data, cols = val_var)

  # order decreasing by val_var
  data.table::setorderv(data, cols = val_var, order = -1L)

  # find SD's for min and max
  results_min <- find_SD(data, "min", n_min, id_var, val_var, by)
  results_max <- find_SD(data, "max", n_max, id_var, val_var, by)

  # generate results DT
  res <- cbind(
    eval(substitute(
      results_min[, .(min = mean(get(val_var)), n_obs_min = .N), keyby = by]
    )),
    eval(substitute(
      results_max[, .(max = mean(get(val_var)), n_obs_max = .N), keyby = by][, c("max", "n_obs_max"), with = FALSE]
    ))
  )
  res <- cbind(data.table::data.table(val_var = val_var), res)

  # check for overlaps of results
  sd_overlap <- nrow(data.table::fintersect(results_min, results_max)) > 0

  if (sd_overlap) {
    message(
      "It is impossible to compute extreme values for variable '",
      val_var, "' that comply to RDC rules."
    )

    for (var in c("min", "max")) {
      data.table::set(res, j = var, value = NA_real_)
    }
    for (var in c("n_obs_min", "n_obs_max")) {
      data.table::set(res, j = var, value = NA_integer_)
    }
    return(invisible(res))
  }
  res
}


#' @importFrom utils tail head
find_SD <- function(data, type, n, id_var, val_var, by) {
  SD_fun <- switch(type,
    min = utils::tail,
    max = utils::head
  )

  SD_results <- find_SD_problems(data, SD_fun, n, id_var, val_var, by)

  while (SD_results[["problems"]]) {
    n <- n + 1L
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
  SD <- eval(substitute(
    data[order(-get(val_var)), SD_fun(.SD, n), by = by],
    env = parent.frame(n = 2L)
  ))

  results_distinct_ids <- eval(eval(substitute(
    check_distinct_ids(SD, id_var, val_var, by),
    env = parent.frame(n = 2L)
  )))
  class(results_distinct_ids) <-
    c("sdc_distinct_ids", class(results_distinct_ids))

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