#' Disclosure control for descriptive statistics
#'
#' @description Checks the number of distinct entities and the (n, k)
#'   dominance rule for your descriptive statistics.
#'
#'   That means that `sdc_descriptives()` checks if there are at least 5
#'   distinct entities and if the largest 2 entities account for 85% or more of
#'   `val_var`. The parameters can be changed using options. For details see
#'   `vignette("options", package = "sdcLog")`.
#'
#' @inheritParams common_arguments
#'
#' @importFrom data.table is.data.table as.data.table set
#' @importFrom cli cli_warn style_bold
#'
#' @import mathjaxr
#'
#' @details
#'   \loadmathjax
#'   The general form of the \mjseqn{(n, k)}-dominance rule can be formulated
#'   as:
#'
#'   \mjsdeqn{\sum_{i=1}^{n}x_i > \frac{k}{100} \sum_{i=1}^{N}x_i}
#'
#'   where \mjseqn{x_1 \ge x_2 \ge \cdots \ge x_{N}}. \mjseqn{n} denotes the
#'   number of largest contributions to be considered, \mjseqn{x_n} the
#'   \mjseqn{n}-th largest contribution, \mjseqn{k} the maximal percentage these
#'   \mjseqn{n} contributions may account for, and \mjseqn{N} is the total
#'   number of observations.
#'
#'   If the statement above is true, the \mjseqn{(n, k)}-dominance rule is
#'   violated.
#'
#' @export
#'
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
#'   by = "sector"
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
#'
#' @return A [list] of class `sdc_descriptives` with detailed information about
#'   options, settings, and compliance with the criteria distinct entities and
#'   dominance.

sdc_descriptives <- function(
        data,
        id_var = getOption("sdc.id_var"),
        val_var = NULL,
        by = NULL,
        zero_as_NA = NULL,
        fill_id_var = FALSE,
        key_vars = getOption("sdc.key_vars")
) {
    distinct_ids <- value_share <- mean_val_var <- NULL # removes NSE notes in R CMD check

    # input checks ----
    checkmate::assert_data_frame(data)
    if (!data.table::is.data.table(data)) {
        data <- data.table::as.data.table(data)
    }
    col_names <- names(data)

    checkmate::assert_string(id_var)
    checkmate::assert_subset(id_var, choices = setdiff(col_names, val_var))

    checkmate::assert_string(val_var, null.ok = TRUE)
    # assert that val_var is not "val_var" (which would lead to errors later on)
    if (!is.null(val_var) && val_var == "val_var") {
        stop("Assertion on 'val_var' failed: Must not equal \"val_var\".")
    }
    checkmate::assert_subset(val_var, choices = setdiff(col_names, c(id_var, key_vars)))

    checkmate::assert_character(by, any.missing = FALSE, null.ok = TRUE)
    checkmate::assert_subset(by, choices = setdiff(col_names, c(id_var, val_var)))

    checkmate::assert_logical(zero_as_NA, len = 1L, null.ok = TRUE)

    checkmate::assert_subset(key_vars, choices = setdiff(col_names, val_var))
    # checkmate::assert_string(time_var, null.ok = TRUE)
    # checkmate::assert_subset(time_var, choices = setdiff(col_names, c(id_var, val_var, key_vars)))
    # check if keys are actually keys
    if (!is.null(key_vars)) {

        # hard check for duplicates by key_vars
        values_unique <- identical(
            unique(data, by = c(key_vars)),
            unique(data, by = c(key_vars, val_var))
        )
        if (isFALSE(values_unique)) {
            stop(
                "Assertion on 'key_vars' failed: Values in 'val_var' are not unique in {'",
                paste0( key_vars, collapse = "','"), "'}."
            )
        }


        # soft check for duplicates by key_vars
        val_var_dups <- anyDuplicated(data[, .SD[1L], by = key_vars], by = val_var)
        # val_var_dups <- anyDuplicated(data, by = c(val_var, time_var, id_var))

        if (val_var_dups > 0L) {
            cli::cli_alert_warning("Duplicates in {.code val_var} detected. Is your specification of {.code key_vars} correct?")
        }

        data[, mean_val_var := get(val_var) / .N, by = key_vars]
        on.exit(set(data, j = "mean_val_var", value = NULL), add = TRUE)
        val_var <- structure("mean_val_var", names = val_var)
        # unique_vals <- data[
        #     j = .N,
        #     by = c(key_vars, time_var, val_var)
        # ][
        #     N > 1L
        # ]
        # # rows_per_keys <- data[, .N, by = c(keys, val_var)][, .N, by = keys][N > 1L]
        # rows_per_keys <- data[
        #     j = list(id_var = unique(get(id_var)), .N),
        #     by = c(keys, val_var)
        # ][
        #     j = .N,
        #     by = "id_var"
        # ][
        #     N > 1L
        # ]
        # if (nrow(rows_per_keys) > 0L) {
        #     stop(
        #         "Assertion on 'keys' failed: The set of 'keys' {'",
        #         paste0(keys, collapse = "', '"), "'} does not uniquely identify each value of 'val_var'."
        #     )
        # }
    }


    # handling 0's ----
    if (!is.null(val_var)) {
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
    }

    if (isTRUE(zero_as_NA)) {
        val_na_idx <- which(data[[val_var]] == 0)
        data.table::set(data, i = val_na_idx, j = val_var, value = NA)

        # reset to zero in order to leave the data unchanged
        on.exit(
            data.table::set(data, i = val_na_idx, j = val_var, value = 0),
            add = TRUE
        )
    }

    # fill id's ----
    if (isTRUE(fill_id_var)) {
        id_na_idx <- which(is.na(data[[id_var]]))
        fill_na(data, id_var, id_na_idx)

        # reset to NA in order to leave the data unchanged
        on.exit(
            data.table::set(data, i = id_na_idx, j = id_var, value = NA),
            add = TRUE
        )
    }

    # check distinct_ids ----
    distinct_ids <- check_distinct_ids(data, id_var, val_var, by)

    # warn about distinct_ids if necessary
    warn_distinct_ids(list(distinct_ids = distinct_ids))

    # check dominance ----
    dominance <- check_dominance(data, id_var, val_var, by, key_vars)

    # warn about dominance if necessary
    if (nrow(
        dominance[value_share >= getOption("sdc.share_dominance", 0.85)]
    ) > 0L) {
        cli::cli_warn(paste(
            cli::style_bold("DISCLOSURE PROBLEM:"),
            "Dominant entities."
        ))
    }

    res <- list(
        options = list_options(),
        settings = list_arguments(id_var, val_var, by, zero_as_NA, fill_id_var, key_vars),
        distinct_ids = distinct_ids,
        dominance = dominance
    )
    class(res) <- c("sdc_descriptives", class(res))
    res
}
