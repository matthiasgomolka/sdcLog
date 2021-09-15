#' Throw only a single warning about insufficient distinct ID's
#' @description Checks if any check on distinct ID's was problematic and throws
#'   a single warning.
#' @param list [list] of elements of class `sdc_distinct_ids`.
#' @return NULL
#' @noRd
warn_distinct_ids <- function(list) {
    distinct_ids <- NULL # removes NSE notes in R CMD check

    problems <- vapply(
        list,
        function(x) {
            var_names <- setdiff(names(x), "distinct_ids")
            x_non_zero <- subset_zero(x, var_names)
            nrow(x_non_zero[distinct_ids < getOption("sdc.n_ids", 5L)]) > 0L
        },
        FUN.VALUE = logical(1L)
    )

    if (sum(problems) > 0L) {
        warning(
            crayon::bold("DISCLOSURE PROBLEM: "),
            "Not enough distinct entities.",
            call. = FALSE
        )
    }
}
