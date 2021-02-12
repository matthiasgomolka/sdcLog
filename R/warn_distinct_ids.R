#' Throw only a single warning about insufficient distinct ID's
#' @description Checks if any check on distinct ID's was problematic and throws
#'   a single warning.
#' @param list [list] of elements of class `sdc_distinct_ids`.
#' @return NULL
warn_distinct_ids <- function(list) {
    distinct_ids <- NULL # removes NSE notes in R CMD check

    problems <- vapply(
        list,
        function(x) nrow(x[distinct_ids < getOption("sdc.n_ids", 5L)]) > 0L,
        FUN.VALUE = logical(1L)
    )

    if (sum(problems) > 0L) {
        warning(
            crayon::bold("POTENTIAL DISCLOSURE PROBLEM: "),
            "Not enough distinct entities.",
            call. = FALSE
        )
    }
}
