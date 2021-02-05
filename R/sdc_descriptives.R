#' Disclosure control for descriptive statistics
#' @description Checks if your descriptive statistics comply to statistical
#'   disclosure control. Checks for number of distinct entities and dominance.
#' @inheritParams common_arguments
#' @importFrom data.table as.data.table set
#' @export
#' @examples
#' sdc_descriptives(
#'   data = sdc_descriptives_DT,
#'   id_var = "id",
#'   val_var = "val_1"
#' )
#'
#' sdc_descriptives(
#'   data = sdc_descriptives_DT,
#'   id_var = "id",
#'   val_var = "val_1",
#'   by = sector
#' )
#'
#' sdc_descriptives(
#'   data = sdc_descriptives_DT,
#'   id_var = "id",
#'   val_var = "val_1",
#'   by = c("sector", "year")
#' )
#'
#' sdc_descriptives(
#'   data = sdc_descriptives_DT,
#'   id_var = "id",
#'   val_var = "val_2",
#'   by = c("sector", "year")
#' )
#'
#' sdc_descriptives(
#'   data = sdc_descriptives_DT,
#'   id_var = "id",
#'   val_var = "val_2",
#'   by = c("sector", "year"),
#'   zero_as_NA = FALSE
#' )
#' @return A [list] of class `sdc_descriptives` with detailed information about
#'   options, settings, and compliance with the criteria distinct entities and
#'   dominance.
sdc_descriptives <- function(data, id_var, val_var = NULL, by = NULL, zero_as_NA = NULL) {
  distinct_ids <- value_share <- NULL # removes NSE notes in R CMD check
  # input checks
  check_args(data, id_var, val_var, by, zero_as_NA)

  data <- data.table::as.data.table(data)

  # handling 0's ----
  if (!is.null(val_var)) {
    share_0 <- data[get(val_var) == 0, .N] / nrow(data)
    zero_as_NA_guess <- share_0 > 0 & !is_dummy(data[[val_var]])

    if (is.null(zero_as_NA)) {
      if (zero_as_NA_guess) {
        zero_as_NA <- TRUE
        message(
          "A share of ",
          signif(share_0, digits = 1L),
          " of 'val_var' are zero. These will be treated as 'NA'.\n",
          "To prevent this behavior and / or avoid this message, set ",
          "'zero_as_NA' explicitly."
        )
      } else {
        zero_as_NA <- FALSE
      }
    }
  }

  if (isTRUE(zero_as_NA)) {
    data.table::set(
      data,
      i = which(data[[val_var]] == 0),
      j = val_var,
      value = NA
    )
  }


  # preprocess by ----
  # (copied to sdc_extreme)
  by <- substitute(by)
  by_name <- by

  # Additional steps if 'by' is not just plain character:
  if (!is.null(by) && !is.character(by)) {

    # Check if 'by' is a list of variable names (either as character or
    # language)
    vars_to_check <- grep(
      "^(\\.|list|c|\\{)$", as.character(by), value = TRUE, invert = TRUE
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
          by <- c(by, temp_name)
        }
      }
    }
  }

  # check distinct_ids ----
  distinct_ids <- structure(
    eval(check_distinct_ids(data, id_var, val_var, by)),
    class = c("sdc_distinct_ids", "data.table", "data.frame")
  )

  # print(distinct_ids)
  if (nrow(distinct_ids[distinct_ids < getOption("sdc.n_ids", 5L)]) > 0L) {
    warning(
      crayon::bold("DISCLOSURE PROBLEM: "),
      "Not enough distinct entities", ".",
      call. = FALSE
    )
  }

  # check dominance ----
  dominance <- structure(
    check_dominance(data, id_var, val_var, by),
    class = c("sdc_dominance", "data.table", "data.frame")
  )

  # print(dominance)
  if (nrow(dominance[value_share >= getOption("sdc.share_dominance", 0.85)]) > 0L) {
    warning(
      crayon::bold("DISCLOSURE PROBLEM: "),
      "Dominant entities.",
      call. = FALSE
    )
  }

  res <- list(
    message_options = message_options(),
    message_arguments = message_arguments(id_var, val_var, by_name, zero_as_NA),
    distinct_ids = distinct_ids,
    dominance = dominance
  )
  class(res) <- c("sdc_descriptives", class(res))
  res
}
