#' Calculate RDC rule-compliant extreme values
#' @description Checks if calculation of extreme values comply to RDC rules. If
#'   so, function returns average min and max values according to RDC rules.
#' @inheritParams common_arguments
#' @importFrom data.table as.data.table data.table setorderv fintersect uniqueN
#'   .N set
#' @importFrom checkmate assert_int
#' @export
#' @examples
#' sdc_extreme(data = sdc_extreme_DT, id_var = "id", val_var = "val_1")
#' sdc_extreme(data = sdc_extreme_DT, id_var = "id", val_var = "val_2")
#' sdc_extreme(data = sdc_extreme_DT, id_var = "id", val_var = "val_2",
#'             n_min = 7)
#' sdc_extreme(data = sdc_extreme_DT, id_var = "id", val_var = "val_3",
#'             n_min = 10, n_max = 10)
#' sdc_extreme(data = sdc_extreme_DT, id_var = "id", val_var = "val_3",
#'             n_min = 8, n_max = 8)
#' sdc_extreme(data = sdc_extreme_DT, id_var = "id", val_var = "val_1",
#'             by = year)
#' sdc_extreme(data = sdc_extreme_DT, id_var = "id", val_var = "val_1",
#'             by = c("sector", "year"))
#' @return A list [list] of class `sdc_extreme` with detailed information about
#'   options, settings and the calculated extreme values (if possible).
sdc_extreme <- function(
  data,
  id_var,
  val_var,
  by = NULL,
  n_min = getOption("sdc.n_ids", 5L),
  n_max = n_min
) {
  # input checks
  check_args(data, id_var, val_var, by)
  checkmate::assert_int(n_max)
  checkmate::assert_int(n_min)

  data <- data.table::as.data.table(data)

  # na.omit.data.table call to prevent NA values being counted
  data <- na.omit(data, cols = val_var)

  # order decreasing by val_var
  data.table::setorderv(data, cols = val_var, order = -1L)

  # preprocess by ----
  # (copied from sdc_descriptives())
  by <- substitute(by)
  by_name <- by

  # Additional steps if 'by' is not just plain character:
  if (!is.null(by) && !is.character(by)) {

    # Check if 'by' is a list of variable names (either as character or
    # language)
    vars_to_check <- grep(
      "^(\\.|list|c)$", as.character(by), value = TRUE, invert = TRUE
    )
    by_name <- paste(vars_to_check, collapse = ", ")

    if (length(vars_to_check) > 0L) {

      # Case: Variable range, like 'sector:year'. Solution: Derive all variables
      # as a character vector
      if (":" %in% vars_to_check) {
        range_vars <- grep(
          ":", vars_to_check, value = TRUE, fixed = TRUE, invert = TRUE
        )
        range_pos <- vapply(
          range_vars,
          function(x) which(x == names(data)),
          FUN.VALUE = integer(1L)
        )
        by <- names(data)[seq(range_pos[1], range_pos[2])]
        by_name <- paste(range_vars, collapse = ", ")


        # Case: 'by' is just a list of variable names. This basically works out
        # of the box.
      } else if (all(vars_to_check %in% names(data))) {

        by_char <- as.character(by)

        # Special subcase: If by is a single bare variable name, like 'sector',
        # it is replaced by "sector". This is necessary to name the by-column
        # correctly.
        if (length(by_char) == 1L) {
          by <- by_char
        }
      } else {

        # Case: At least one element of by is an expression

        # extract variables from by/vars_to_check and insert into new 'by'
        is_in_data <- vars_to_check %in% names(data)
        by <- vars_to_check[is_in_data]
        by_name <- by

        # Loop over all expressions and create temporary variables before
        # slicing the data. Also, add these to the new 'by' and add their
        # expressions to 'by_name'.
        expr_not_in_data <- vars_to_check[!is_in_data]

        # init temporary variable names
        temp_num <- 1L
        temp_name <- paste0("tmp_", temp_num)

        for (expr in expr_not_in_data) {
          by_name <- paste(by_name, as.character(as.expression(expr)), sep = ", ")

          # make sure that tmp_* does not yet exist
          while (temp_name %in% names(data)) {
            temp_num <- temp_num + 1L
            temp_name <- paste0("tmp_", temp_num)
          }
          data[, c(temp_name) := eval(str2lang(expr))]
          # by <- temp_name
          by <- c(by, temp_name)
        }
      }
    }
  }

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
  keep_max <- setdiff(names(res_max), names(res_min))

  # combine results
  res <- cbind(
    data.table(val_var = val_var),
    res_min,
    res_max[, keep_max, with = FALSE]
  )

  # set expressions as variable names
  if (exists("expr_not_in_data")) {
    for (i in seq_along(expr_not_in_data)) {
      data.table::setnames(res, old = paste0("tmp_", i), new = expr_not_in_data[i])
    }
  }
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
      message_arguments = message_arguments(id_var, val_var, by_name),
      min_max = res
    ),
    class = c("sdc_extreme", "list")
  )
}


#' @importFrom utils tail head
find_SD <- function(data, type, n_obs, id_var, val_var, by) {
  SD_fun <- switch(type,
                   min = utils::tail,
                   max = utils::head
  )

  SD_results <- find_SD_problems(data, SD_fun, n_obs, id_var, val_var, by)

  while (SD_results[["problems"]]) {
    n_obs <- n_obs + 1L
    SD_results <- find_SD_problems(data, SD_fun, n_obs, id_var, val_var, by)

    # this assures that this is no infinite loop; problems will be catched
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
