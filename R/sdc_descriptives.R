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
sdc_descriptives <- function(data, id_var, val_var, by = NULL, zero_as_NA = NULL) {
  distinct_ids <- value_share <- NULL # removes NSE notes in R CMD check
  # input checks
  check_args(data, id_var, val_var, by, zero_as_NA)

  data <- data.table::as.data.table(data)

  # handling 0's
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

  if (isTRUE(zero_as_NA)) {
    data.table::set(
      data,
      i = which(data[[val_var]] == 0),
      j = val_var,
      value = NA
    )
  }

  # best guess NA's
  # possible_na_df <-
  #     data[ , `:=`(count = .N) , by = val_var
  #        ][which.max(count)
  #        ][, list(possible_na = get(val_var),
  #                 value_share = count/length(data[[val_var]]))
  #        ][value_share >= getOption("sdc.share_possible_na", 0.20)]
  #
  # if (nrow(possible_na_df) > 0 && !is.na(possible_na_df[[1]])) {
  #     message("The value '", possible_na_df[[1, 1]], "' occurs frequently in the data: Is it used as coding for NA?")}


  # check distinct_ids ----
  expr_distinct_ids <- eval(substitute(
    check_distinct_ids(data, id_var, val_var, by)
  ))
  distinct_ids <- structure(
    eval(expr_distinct_ids),
    class = c("sdc_distinct_ids", "data.table", "data.frame")
  )

  # print(distinct_ids)
  if (nrow(distinct_ids[distinct_ids < getOption("sdc.n_ids", 5L)]) > 0L) {
    warning(
      crayon::bold("Potential disclosure problem: "),
      "Not enough distinct entities", ".",
      call. = FALSE
    )
  }

  # check dominance ----
  expr_dominance <- eval(substitute(
    check_dominance(data, id_var, val_var, by)
  ))
  dominance <- structure(
    eval(expr_dominance),
    class = c("sdc_dominance", "data.table", "data.frame")
  )

  # print(dominance)
  if (nrow(dominance[value_share >= getOption("sdc.share_dominance", 0.85)]) > 0L) {
    warning(
      crayon::bold("Potential disclosure problem: "),
      "Dominant entities", ".",
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
