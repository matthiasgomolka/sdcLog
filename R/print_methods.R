#' @importFrom crayon bold red
print.sdc_counts <- function(res) {
    msg <- "Not enought distinct entities"

    # with problems
    if (nrow(res) > 0L) {
        warning(
            crayon::bold("Potential disclosure problem: "), msg, ".",
            call. = FALSE
        )
        msg <- paste0(msg, ":\n")
        cat(crayon::red(msg))
        print(as.data.table(res))

    # no problems
    } else if (getOption("sdc.info_level", 1L) > 1L) {
        message("No problem with number of distinct entities.")
    }
}

#' @importFrom crayon bold red
print.sdc_dominance <- function(res) {
    msg <- "Dominant entities"

    # with problems
    if (nrow(res) > 0L) {
        warning(
            crayon::bold("Potential disclosure problem: "), msg, ".",
            call. = FALSE
        )
        msg <- paste0(msg, ":\n")
        cat(crayon::red(msg))
        print(as.data.table(res))

    # no problems
    } else if (getOption("sdc.info_level", 1L) > 1L) {
        message("No problem with dominance.")
    }
}

print.sdc_descriptives <- function(res) {
    no_problems <- sum(nrow(res[["counts"]]), nrow(res[["dominance"]])) == 0L
    if (no_problems & (getOption("sdc.info_level", 1L) > 0L)) {
        message("Output complies to RDSC rules.")
    }
}


print.sdc_model <- function(res) {
    n_problems <- vapply(c("dominance_list", "dummy_list"), function(lst) {
        sum(vapply(res[[lst]], nrow, FUN.VALUE = integer(1L)))
    }, FUN.VALUE = integer(1L))
    no_problems <- sum(nrow(res[["distinct_ids"]]), n_problems) == 0L

    if (no_problems & (getOption("sdc.info_level", 1L) > 0L)) {
        message("Output complies to RDSC rules.")
    }
}

