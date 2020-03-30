#' Check if your descriptive statistics comply to statistical disclosure control
#' @param data [data.frame] from which the descriptives are calculated.
#' @param id_var [character] The name of the id variable.
#' @param val_var [character] vector of value variables on which descriptives
#'   are computed.
#' @param by Grouping variables (or expression). Can be provided as in
#'   [data.table::data.table()].
#' @importFrom data.table as.data.table
#' @export

sdc_descriptives <- function(data, id_var, val_var, by = NULL) {
    # input checks
    check_args(data, id_var, val_var, by)

    # status messages
    message_options()
    message_arguments(id_var, val_var, by)

    data <- data.table::as.data.table(data)

    # check counts
    expr_counts <- eval(substitute(
        check_distinct_ids(data, id_var, val_var, by)
    ))
    counts <- eval(expr_counts)
    class(counts) <- c("sdc_counts", class(counts))

    # print(counts)
    if (nrow(counts) > 0L) {
        warning(
            crayon::bold("Potential disclosure problem: "),
            "Not enough distinct entities", ".",
            call. = FALSE
        )
    }

    # check dominance
    expr_dominance <- eval(substitute(
        check_dominance(data, id_var, val_var, by)
    ))
    dominance <- eval(expr_dominance)
    class(dominance) <- c("sdc_dominance", class(dominance))

    # print(dominance)
    if (nrow(dominance) > 0L) {
        warning(
            crayon::bold("Potential disclosure problem: "),
            "Dominant entities", ".",
            call. = FALSE
        )
    }

    res <- list(distinct_ids = counts, dominance = dominance)
    class(res) <- c("sdc_descriptives", class(res))
    res
}
