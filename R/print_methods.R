#' @importFrom crayon bold red
#' @importFrom data.table as.data.table between
#' @export
print.sdc_distinct_ids <- function(x, ...) {
  distinct_ids <- NULL # removes NSE notes in R CMD check

  var_names <- setdiff(names(x), "distinct_ids")
  x_non_zero <- subset_zero(x, var_names)

  # with problems
  if (nrow(x_non_zero[data.table::between(
    distinct_ids,
    lower = 0L,
    upper = getOption("sdc.n_ids", 5L),
    incbounds = FALSE)]
  ) > 0L) {
    cat(crayon::red("Not enough distinct entities:\n"))
    print(data.table::as.data.table(x))

    # withOUT problems
  } else if (getOption("sdc.info_level", 1L) > 1L) {

    n_distinct_ids <- min(x[["distinct_ids"]])
    message(
      "No problem with number of distinct entities (", n_distinct_ids, ")."
    )
  }

}

#' @importFrom crayon bold red
#' @importFrom data.table as.data.table
#' @export
print.sdc_dominance <- function(x, ...) {
  value_share <- NULL # removes NSE notes in R CMD check

  # with problems
  if (nrow(x[value_share >= getOption("sdc.share_dominance", 0.85)]) > 0L) {
    cat(crayon::red("Dominant entities:\n"))
    print(data.table::as.data.table(x))


    # withOUT problems
  } else if (getOption("sdc.info_level", 1L) > 1L) {

    # without dominance check (i.e. val_var = NULL in sdc_descriptives())
    dt_no_check <- structure(
      data.table::data.table(value_share = NA_real_),
      class = c("sdc_dominance", "data.table", "data.frame")
    )

    if (identical(x, dt_no_check)) {
      message("No dominance check conducted, because 'val_var = NULL'.")

      # regular case without problems
    } else {
      message("No problem with dominance.")
    }
  }

}


#' @importFrom crayon bold underline
#' @export
print.sdc_options <- function(x, ...) {
  message(
    crayon::bold("OPTIONS: "),
    paste(names(x), crayon::underline(x), sep = ": ", collapse = " | ")
  )
}


#' @importFrom crayon bold underline
#' @export
print.sdc_settings <- function(x, ...) {
  x <- x[!vapply(x, is.null, FUN.VALUE = logical(1L))]

  message(
    crayon::bold("SETTINGS: "),
    paste(names(x), crayon::underline(x), sep = ": ", collapse = " | ")
  )
}

#' @importFrom data.table between
#' @export
print.sdc_descriptives <- function(x, ...) {
  distinct_ids <- value_share <- NULL # removes NSE notes in R CMD check

  print(x[["options"]])
  print(x[["settings"]])

  print(x[["distinct_ids"]])
  print(x[["dominance"]])
  no_problems <- sum(
    nrow(x[["distinct_ids"]][data.table::between(
      distinct_ids,
      lower = 0L,
      upper = getOption("sdc.n_ids", 5L),
      incbounds = FALSE)]),
    nrow(
      x[["dominance"]][value_share >= getOption("sdc.share_dominance", 0.85)]
    )
  ) == 0L
  if (no_problems & (getOption("sdc.info_level", 1L) > 0L)) {
    message("Output complies to RDC rules.")
  }
}

#' @export
print.sdc_model <- function(x, ...) {
  distinct_ids <- NULL # removes NSE notes in R CMD check

  print(x[["options"]])
  print(x[["settings"]])

  print(x[["distinct_ids"]])
  if (getOption("sdc.info_level", 1L) <= 1L) {
    print_fun <- conditional_print
  } else {
    print_fun <- print
  }

  if (length(x[["terms"]]) != 0) {
    print_fun(x[["terms"]])
  }

  n_problems <- vapply(
    append(list(distinct_ids = x[["distinct_ids"]]), x[["terms"]]),
    function(x) {
      var_names <- setdiff(names(x), "distinct_ids")
      x_non_zero <- subset_zero(x, var_names)
      nrow(x_non_zero[data.table::between(
        distinct_ids,
        lower = 0L,
        upper = getOption("sdc.n_ids", 5L),
        incbounds = FALSE)])
    },
    FUN.VALUE = integer(1L)
  )
  no_problems <- sum(n_problems) == 0L

  if (no_problems & (getOption("sdc.info_level", 1L) > 0L)) {
    message("Output complies to RDC rules.")
  }
}

#' @export
print.sdc_min_max <- function(x, ...) {
  print(x[["options"]])
  print(x[["settings"]])
  print(x[["min_max"]])
}
