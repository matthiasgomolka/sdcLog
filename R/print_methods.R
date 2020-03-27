#' @importFrom crayon bold red
#' @export
print.sdc_counts <- function(x, ...) {
    msg <- "Not enough distinct entities"

    # with problems
    if (nrow(x) > 0L) {
        warning(
            crayon::bold("Potential disclosure problem: "), msg, ".",
            call. = FALSE
        )
        msg <- paste0(msg, ":\n")
        cat(crayon::red(msg))
        print(as.data.table(x))

    # no problems
    } else if (getOption("sdc.info_level", 1L) > 1L) {
        message("No problem with number of distinct entities.")
    }
}

#' @importFrom crayon bold red
#' @export
print.sdc_dominance <- function(x, ...) {
    msg <- "Dominant entities"

    # with problems
    if (nrow(x) > 0L) {
        warning(
            crayon::bold("Potential disclosure problem: "), msg, ".",
            call. = FALSE
        )
        msg <- paste0(msg, ":\n")
        cat(crayon::red(msg))
        print(as.data.table(x))

    # no problems
    } else if (getOption("sdc.info_level", 1L) > 1L) {
        message("No problem with dominance.")
    }
}


#' @export
print.sdc_descriptives <- function(x, ...) {
    no_problems <- sum(nrow(x[["counts"]]), nrow(x[["dominance"]])) == 0L
    if (no_problems & (getOption("sdc.info_level", 1L) > 0L)) {
        message("Output complies to RDSC rules.")
    }
}


#' @export
print.sdc_model <- function(x, ...) {
    n_problems <- vapply(c("dominance_list", "dummy_list"), function(lst) {
        sum(vapply(x[[lst]], nrow, FUN.VALUE = integer(1L)))
    }, FUN.VALUE = integer(1L))
    no_problems <- sum(nrow(x[["distinct_ids"]]), n_problems) == 0L

    if (no_problems & (getOption("sdc.info_level", 1L) > 0L)) {
        message("Output complies to RDSC rules.")
    }
}

