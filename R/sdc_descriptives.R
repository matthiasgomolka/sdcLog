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
#'   by = "sector"
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
sdc_descriptives <- function(data, id_var = getOption("sdc.id_var"), val_var = NULL, by = NULL, zero_as_NA = NULL) {
  distinct_ids <- value_share <- NULL # removes NSE notes in R CMD check

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

  checkmate::assert_logical(zero_as_NA, len = 1L, null.ok = TRUE)


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


  # check distinct_ids ----
  distinct_ids <- check_distinct_ids(data, id_var, val_var, by)

  # warn about distinct_ids if necessary
  warn_distinct_ids(list(distinct_ids = distinct_ids))

  # check dominance ----
  dominance <- check_dominance(data, id_var, val_var, by)

  # warn about dominance if necessary
  if (nrow(dominance[value_share >= getOption("sdc.share_dominance", 0.85)]) > 0L) {
    warning(
      crayon::bold("DISCLOSURE PROBLEM: "), "Dominant entities.",
      call. = FALSE
    )
  }

  res <- list(
    message_options = message_options(),
    message_arguments = message_arguments(id_var, val_var, by, zero_as_NA),
    distinct_ids = distinct_ids,
    dominance = dominance
  )
  class(res) <- c("sdc_descriptives", class(res))
  res
}
