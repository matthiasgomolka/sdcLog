#' Calculate RDC rule-compliant extreme values
#' @description Checks if calculation of extreme values comply to RDC rules. If
#'   so, function returns average min and max values according to RDC rules.
#' @inheritParams common_arguments
#' @importFrom data.table as.data.table data.table setorderv fintersect uniqueN
#'   .N set
#' @importFrom checkmate assert_int
#' @export
#' @examples
#' sdc_extreme(sdc_extreme_DT, id_var = "id", val_var = "val_1")
#' sdc_extreme(sdc_extreme_DT, id_var = "id", val_var = "val_2")
#' sdc_extreme(sdc_extreme_DT, id_var = "id", val_var = "val_2", n_min = 7)
#' sdc_extreme(
#'   sdc_extreme_DT, id_var = "id", val_var = "val_3", n_min = 10, n_max = 10
#' )
#' sdc_extreme(
#'   sdc_extreme_DT, id_var = "id", val_var = "val_3", n_min = 8, n_max = 8
#' )
#' sdc_extreme(
#'   sdc_extreme_DT, id_var = "id", val_var = "val_1", by = "year"
#' )
#' sdc_extreme(
#'   sdc_extreme_DT, id_var = "id", val_var = "val_1", by = c("sector", "year")
#' )
#' @return A list [list] of class `sdc_extreme` with detailed information about
#'   options, settings and the calculated extreme values (if possible).
sdc_extreme <- function(
  data,
  id_var = getOption("sdc.id_var"),
  val_var,
  by = NULL,
  n_min = getOption("sdc.n_ids", 5L),
  n_max = n_min
) {
  # input checks ----
  checkmate::assert_data_frame(data)
  col_names <- names(data)

  checkmate::assert_string(id_var)
  checkmate::assert_subset(id_var, choices = names(data))

  checkmate::assert_string(val_var, null.ok = TRUE)
  checkmate::assert_subset(val_var, choices = names(data))

  checkmate::assert_character(by, any.missing = FALSE, null.ok = TRUE)
  checkmate::assert_subset(by, choices = names(data))

  checkmate::assert_int(n_max)
  checkmate::assert_int(n_min)

  data <- data.table::as.data.table(data)

  # na.omit.data.table call to prevent NA values being counted
  data <- na.omit(data, cols = val_var)

  # order decreasing by val_var
  data.table::setorderv(data, cols = val_var, order = -1L)

  # find SD's for min and max
  SD_min <- find_SD(data, "min", n_min, id_var, val_var, by)
  res_min <- SD_min[
    j = list(
      min = mean(get(val_var)),
      distinct_ids_min = data.table::uniqueN(get(id_var))
    ),
    keyby = by
  ]

  SD_max <- find_SD(data, "max", n_max, id_var, val_var, by)
  res_max <- SD_max[
    j = list(
      max = mean(get(val_var)),
      distinct_ids_max = data.table::uniqueN(get(id_var))
    ),
    keyby = by
  ]

  # combine results
  res <- cbind(
    data.table(val_var = val_var),
    res_min,
    res_max[, setdiff(names(res_max), names(res_min)), with = FALSE]
  )

  # check for overlaps of results
  sd_overlap <- nrow(data.table::fintersect(SD_min, SD_max)) > 0

  if (sd_overlap) {
    for (var in c("min", "max")) {
      data.table::set(res, j = var, value = NA_real_)
    }
    for (var in c("distinct_ids_min", "distinct_ids_max")) {
      data.table::set(res, j = var, value = NA_integer_)
    }
  }

  structure(
    list(
      message_options = message_options(),
      message_arguments = message_arguments(id_var, val_var, by),
      min_max = res
    ),
    class = c("sdc_extreme", "list")
  )
}


#' @importFrom utils tail head
find_SD <- function(data, type, n_obs, id_var, val_var, by) {
  SD_fun <- switch(type, min = utils::tail, max = utils::head)

  SD_results <- find_SD_problems(data, SD_fun, n_obs, id_var, val_var, by)

  while (SD_results[["problems"]]) {
    n_obs <- n_obs + 1L
    SD_results <- find_SD_problems(data, SD_fun, n_obs, id_var, val_var, by)

    # this assures that this is no infinite loop; problems will be caught
    # during the check for overlaps
    if (n_obs >= nrow(data)) {
      return(SD_results[["SD"]])
    }
  }

  return(SD_results[["SD"]])
}

#' @importFrom data.table .SD
find_SD_problems <- function(data, SD_fun, n_obs, id_var, val_var, by) {
  distinct_ids <- value_share <- NULL # removes NSE notes in R CMD check

  SD <- data[order(-get(val_var)), SD_fun(.SD, n_obs), by = by]

  results_distinct_ids <- structure(
    eval(check_distinct_ids(SD, id_var, val_var, by)),
    class = c("sdc_distinct_ids", "data.table", "data.frame")
  )

  results_dominance <- structure(
    check_dominance(SD, id_var, val_var, by),
    class =  c("sdc_dominance", "data.table", "data.frame")
  )

  list(
    SD = SD,
    problems = sum(
      nrow(results_distinct_ids[distinct_ids < getOption("sdc.n_ids", 5L)]),
      nrow(results_dominance[value_share >= getOption("sdc.share_dominance", 0.85)])
    ) != 0L
  )
}
