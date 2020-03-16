#' @importFrom crayon bold red
print.sdc_counts <- function(res) {
    if (nrow(res) > 0L) {
        warning(crayon::bold(
            "Potential disclosure problem: Not enought distinct entities."),
            call. = FALSE
        )
        cat(crayon::red(
            "Not enough distinct entities:\n"
        ))
        print(as.data.table(res))
    } else if (getOption("sdc.verbose", TRUE)) {
        message("No problem with number of distinct entities.")
    }
}

#' @importFrom crayon bold red
print.sdc_dominance <- function(res) {
    if (nrow(res) > 0L) {
        warning(crayon::bold(
            "Potential disclosure problem: Dominant entities."),
            call. = FALSE
        )
        cat(crayon::red("Dominant entities:\n"))
        print(as.data.table(res))
    } else {
        message("No problem with dominance.")
    }
}


# option no print for no problems

#' @importFrom crayon bold red
print.sdc_counts <- function(res) {
    if (nrow(res) > 0L) {
        warning(crayon::bold(
            "Potential disclosure problem: Not enought distinct entities."),
            call. = FALSE
        )
        cat(crayon::red(
            "Not enough distinct entities:\n"
        ))
        print(as.data.table(res))
    }
}

#' @importFrom crayon bold red
print.sdc_dominance <- function(res) {
    if (nrow(res) > 0L) {
        warning(crayon::bold(
            "Potential disclosure problem: Dominant entities."),
            call. = FALSE
        )
        cat(crayon::red("Dominant entities:\n"))
        print(as.data.table(res))
    }
}



print.sdc <- function(res) {
    if (sum(nrow(res[["counts"]]), nrow(res[["dominance"]])) == 0) {
        message("Output complies to RDSC rules.")
    } else if (nrow(res[["counts"]]) == 0) {
        message("No problem with number of distinct entities.")
    } else if (nrow(res[["dominance"]]) == 0) {
        message("No problem with dominance.")
    }
}




