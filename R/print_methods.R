#' Print methods for SDC objects
#'
#' @description These methods print SDC objects. Tables containing information
#'   are only printed when relevant.
#'
#' @param x The object to be printed
#' @param ... Ignored.
#'
#' @importFrom cli cli_alert_danger cli_alert_success
#' @importFrom data.table as.data.table between
#'
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
        cli::cli_alert_danger("Not enough distinct entities:\n")
        print(data.table::as.data.table(x))

        # withOUT problems
    } else if (getOption("sdc.info_level", 1L) > 1L) {

        n_distinct_ids <- min(x[["distinct_ids"]])
        cli::cli_alert_success(
            "No problem with number of distinct entities ({n_distinct_ids})."
        )
    }

}

#' @importFrom cli cli_alert_danger cli_alert_info cli_alert_success
#' @importFrom data.table as.data.table
#' @rdname print.sdc_distinct_ids
#' @export
print.sdc_dominance <- function(x, ...) {
    value_share <- NULL # removes NSE notes in R CMD check

    # with problems
    if (nrow(x[value_share >= getOption("sdc.share_dominance", 0.85)]) > 0L) {
        cli::cli_alert_danger("Dominant entities:\n")
        print(data.table::as.data.table(x))


        # withOUT problems
    } else if (getOption("sdc.info_level", 1L) > 1L) {

        # without dominance check (i.e. val_var = NULL in sdc_descriptives())
        dt_no_check <- structure(
            data.table::data.table(value_share = NA_real_),
            class = c("sdc_dominance", "data.table", "data.frame")
        )

        if (identical(x, dt_no_check)) {
            cli::cli_alert_info("No dominance check conducted, because 'val_var = NULL'.")

            # regular case without problems
        } else {
            if (nrow(x) > 0L) {
                max_dominance <- round(max(x[["value_share"]]), digits = 2)
                max_dominance_info <- paste0(" (", max_dominance, ")")
            } else {
                max_dominance_info <- ""
            }
            cli::cli_alert_success("No problem with dominance{max_dominance_info}.")
        }
    }

}


#' @importFrom cli cli_text style_bold
#' @rdname print.sdc_distinct_ids
#' @export
print.sdc_options <- function(x, ...) {
    cli::cli_text(paste(
        cli::style_bold("OPTIONS:"),
        paste(names(x), cli::style_bold(x), sep = ": ", collapse = " | ")
    ))
}


#' @importFrom cli cli_text style_bold
#' @rdname print.sdc_distinct_ids
#' @export
print.sdc_settings <- function(x, ...) {
    x <- x[!vapply(x, is.null, FUN.VALUE = logical(1L))]

    cli::cli_text(paste(
        cli::style_bold("SETTINGS:"),
        paste(names(x), cli::style_bold(x), sep = ": ", collapse = " | ")
    ))
}

#' @importFrom cli cli_alert_success
#' @importFrom data.table between
#' @rdname print.sdc_distinct_ids
#' @export
print.sdc_descriptives <- function(x, ...) {
    distinct_ids <- value_share <- NULL # removes NSE notes in R CMD check

    title_rule("SDC results (descriptives)")

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
        cli::cli_alert_success("Output complies to RDC rules.")
    }
    end_rule()
}

#' @importFrom cli cli_alert_success
#' @rdname print.sdc_distinct_ids
#' @export
print.sdc_model <- function(x, ...) {
    distinct_ids <- NULL # removes NSE notes in R CMD check

    title_rule("SDC results (model)")
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
        cli::cli_alert_success("Output complies to RDC rules.")
    }
    end_rule()
}

#' importFrom cli cli_alert_info
#' @rdname print.sdc_distinct_ids
#' @export
print.sdc_min_max <- function(x, ...) {
    title_rule("SDC safe min/max")
    print(x[["options"]])
    print(x[["settings"]])
    if (is.na(x[["min_max"]][[1L, "min"]])) {
        cli::cli_alert_info(
            "It is impossible to compute extreme values for variable '{x[['min_max']][[1L, 'val_var']]}' that comply to RDC rules."
        )
    }
    print(x[["min_max"]])
    end_rule()
}


#' Helper function to print specific title rules
#' @importFrom cli rule style_bold
#' @keywords internal
#' @noRd
title_rule <- function(title) {
    print(cli::rule(
        right = cli::style_bold(title),
        line_col = "cyan"
    ))
}

#' Helper function to print specific end rules
#' @importFrom cli rule
#' @keywords internal
#' @noRd
end_rule <- function() {
    print(cli::rule(col = "cyan"))
}
