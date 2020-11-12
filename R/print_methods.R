#' @importFrom crayon bold red
#' @importFrom data.table as.data.table
#' @export
print.sdc_distinct_ids <- function(x, ...) {
  distinct_ids <- NULL # removes NSE notes in R CMD check
  # with problems
  if (nrow(x[distinct_ids < getOption("sdc.n_ids", 5L)]) > 0L) {
    cat(crayon::red("Not enough distinct entities:\n"))
    print(data.table::as.data.table(x))

  # withOUT problems
  } else if (getOption("sdc.info_level", 1L) > 1L) {
    message("No problem with number of distinct entities.")
  }

}

#' @importFrom crayon bold red
#' @importFrom data.table as.data.table
#' @export
print.sdc_dominance <- function(x, ...) {
  distinct_ids <- value_share <- NULL # removes NSE notes in R CMD check
  # with problems
  if (nrow(x[value_share >= getOption("sdc.share_dominance", 0.85)]) > 0L) {
    cat(crayon::red("Dominant entities:\n"))
    print(data.table::as.data.table(x))

  # withOUT problems
  } else if (getOption("sdc.info_level", 1L) > 1L) {
    message("No problem with dominance.")
  }

}


#' @export
print.sdc_descriptives <- function(x, ...) {
  distinct_ids <- value_share <- NULL # removes NSE notes in R CMD check
  message(x[["message_options"]])
  message(x[["message_arguments"]])

  print(x[["distinct_ids"]])
  print(x[["dominance"]])
  no_problems <- sum(
    nrow(x[["distinct_ids"]][distinct_ids < getOption("sdc.n_ids", 5L)]),
    nrow(x[["dominance"]][value_share >= getOption("sdc.share_dominance", 0.85)])
  ) == 0L
  if (no_problems & (getOption("sdc.info_level", 1L) > 0L)) {
    message("Output complies to RDC rules.")
  }
}


#' @export
print.sdc_model <- function(x, ...) {
  distinct_ids <- NULL # removes NSE notes in R CMD check
  message(x[["message_options"]])
  message(x[["message_arguments"]])

  print(x[["distinct_ids"]])
  if (getOption("sdc.info_level", 1L) <= 1L) {
    print_fun <- conditional_print
  } else {
    print_fun <- print
  }

  if (length(x[["dummies"]]) != 0) {
    print_fun(x[["dummies"]])
  }
  if (length(x[["interactions"]]) != 0) {
    print_fun(x[["interactions"]])
  }

  n_problems <- vapply(
    c(x[["dummies"]], x[["interactions"]]),
    function(x) nrow(x[distinct_ids < getOption("sdc.n_ids", 5L)]),
    FUN.VALUE = integer(1L)
  )
  no_problems <- sum(n_problems) == 0L

  if (no_problems & (getOption("sdc.info_level", 1L) > 0L)) {
    message("Output complies to RDC rules.")
  }
}
