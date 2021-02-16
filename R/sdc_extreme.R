#' Calculate RDC rule-compliant extreme values
#' @description Checks if calculation of extreme values comply to RDC rules. If
#'   so, function returns average min and max values according to RDC rules.
#' @inheritParams common_arguments
#' @importFrom data.table as.data.table data.table setorderv fintersect uniqueN
#'   .N set
#' @importFrom checkmate assert_int
#' @export
#' @examples
#' sdc_min_max(sdc_min_max_DT, id_var = "id", val_var = "val_1")
#' sdc_min_max(sdc_min_max_DT, id_var = "id", val_var = "val_2")
#' sdc_min_max(sdc_min_max_DT, id_var = "id", val_var = "val_3", max_obs = 10)
#' sdc_min_max(sdc_min_max_DT, id_var = "id", val_var = "val_1", by = "year")
#' sdc_min_max(
#'   sdc_min_max_DT, id_var = "id", val_var = "val_1", by = c("sector", "year")
#' )
#' @return A list [list] of class `sdc_min_max` with detailed information about
#'   options, settings and the calculated extreme values (if possible).
sdc_min_max <- function(
  data,
  id_var = getOption("sdc.id_var"),
  val_var,
  by = NULL,
  max_obs = nrow(data)
) {
  # input checks ----
  checkmate::assert_data_frame(data)
  data <- data.table::as.data.table(data)
  col_names <- names(data)

  checkmate::assert_string(id_var)
  checkmate::assert_subset(id_var, choices = col_names)

  checkmate::assert_string(val_var, null.ok = TRUE)
  checkmate::assert_subset(val_var, choices = setdiff(col_names, id_var))

  checkmate::assert_character(by, any.missing = FALSE, null.ok = TRUE)
  checkmate::assert_subset(by, choices = setdiff(col_names, c(id_var, val_var)))

  min_obs <- getOption("sdc.n_ids", 5L)
  checkmate::assert_int(max_obs, lower = min_obs, upper = nrow(data))


  # na.omit.data.table call to prevent NA values being counted
  data <- na.omit(data, cols = val_var)

  # order decreasing by val_var
  data.table::setorderv(data, cols = val_var, order = -1L)

  # find SD's for min and max
  SD_min <- find_SD(data, "min", min_obs, max_obs, id_var, val_var, by)
  res_min <- SD_min[
    j = list(
      min = mean(get(val_var)),
      distinct_ids_min = data.table::uniqueN(get(id_var))
    ),
    keyby = by
  ]

  SD_max <- find_SD(data, "max", min_obs, max_obs, id_var, val_var, by)
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

  if (is.na(res[[1L, "min"]])) {
    message(
      "It is impossible to compute extreme values for variable '",
      res[[1L, "val_var"]], "' that comply to RDC rules."
    )
  }

  structure(
    list(
      message_options = message_options(),
      message_arguments = message_arguments(id_var, val_var, by),
      min_max = res
    ),
    class = c("sdc_min_max", "list")
  )
}


#' @importFrom utils tail head
find_SD <- function(data, type, n_obs, max_obs, id_var, val_var, by) {
  SD_fun <- switch(type, min = utils::tail, max = utils::head)

  SD_results <- find_SD_problems(data, SD_fun, n_obs, id_var, val_var, by)

  while (SD_results[["problems"]]) {
    n_obs <- n_obs + 1L
    SD_results <- find_SD_problems(data, SD_fun, n_obs, id_var, val_var, by)

    # this assures that if n_obs >= max_obs, the loop will break; problems will
    # be caught during the check for overlaps
    if (n_obs >= max_obs) {
      return(data)
    }

  }

  return(SD_results[["SD"]])
}

#' @importFrom data.table .SD
find_SD_problems <- function(data, SD_fun, n_obs, id_var, val_var, by) {
  distinct_ids <- value_share <- NULL # removes NSE notes in R CMD check

  SD <- data[order(-get(val_var)), SD_fun(.SD, n_obs), by = by]

  distinct_ids <- check_distinct_ids(SD, id_var, val_var, by)

  dominance <- check_dominance(SD, id_var, val_var, by)

  list(
    SD = SD,
    problems = sum(
      nrow(distinct_ids[distinct_ids < getOption("sdc.n_ids", 5L)]),
      nrow(dominance[value_share >= getOption("sdc.share_dominance", 0.85)])
    ) != 0L
  )
}
