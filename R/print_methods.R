#' @importFrom crayon bold red
#' @export
print.sdc_distinct_ids <- function(x, ...) {
    msg <- "Not enough distinct entities"

    # with problems
    if (nrow(x) > 0L) {
        # warning(
        #     crayon::bold("Potential disclosure problem: "), msg, ".",
        #     call. = FALSE
        # )
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
        #warning(
        #   crayon::bold("Potential disclosure problem: "), msg, ".",
        #   call. = FALSE
        #)
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
    message(x[["message_options"]])
    message(x[["message_arguments"]])

    print(x[["distinct_ids"]])
    print(x[["dominance"]])
    no_problems <- sum(nrow(x[["distinct_ids"]]), nrow(x[["dominance"]])) == 0L
    if (no_problems & (getOption("sdc.info_level", 1L) > 0L)) {
        message("Output complies to RDSC rules.")
    }
}


#' @export
print.sdc_model <- function(x, ...) {
    message(x[["message_options"]])
    message(x[["message_arguments"]])

    print(x[["distinct_ids"]])
    if (getOption("sdc.info_level", 1L) <= 1L) {
        print_fun <- conditional_print
    } else {
        print_fun <- print
    }
    # print_fun <- switch(getOption("sdc.info_level", 1L),
    #     `0` = conditional_print(),
    #     `1` = conditional_print(),
    #     `2` = print()
    # )
    print_fun(x[["dominance_list"]])

    if (length(x[["dummy_list"]]) != 0){
        print_fun(x[["dummy_list"]])
    }

    n_problems <- vapply(c("dominance_list", "dummy_list"), function(lst) {
        sum(vapply(x[[lst]], nrow, FUN.VALUE = integer(1L)))
    }, FUN.VALUE = integer(1L))
    no_problems <- sum(nrow(x[["distinct_ids"]]), n_problems) == 0L

    if (no_problems & (getOption("sdc.info_level", 1L) > 0L)) {
        message("Output complies to RDSC rules.")
    }
}

