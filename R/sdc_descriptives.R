#' Check if your descriptive statistics comply to statistical disclosure control.
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
    sdc_arg_check(data, id_var, val_var, by)

    # status messages
    message_options()
    message_arguments(id_var, val_var, by)

    data <- data.table::as.data.table(data)

    # check counts
    expr_counts <- eval(substitute(sdc_count(data, id_var, val_var, by)))
    counts <- eval(expr_counts)
    class(counts) <- c("sdc_counts", class(counts))
    print(counts)

    # check dominance
    expr_dominance <- eval(substitute(sdc_dominance(data, id_var, val_var, by)))
    dominance <- eval(expr_dominance)
    class(dominance) <- c("sdc_dominance", class(dominance))
    print(dominance)

    res <- list(counts = counts, dominance = dominance)
    class(res) <- c("sdc", class(res))
    res
}

#' Internal function which creates cross-tables with number of distinct id's
#' @param DT The data.table from which the descriptives are calculated.
#' @param id_var The name of the id variable. Can be provided as bare (unquoted)
#'   column name or as a character.
#' @param val_var The name of the value variable. Can be provided as bare
#'   (unquoted) column name or as a character.
#' @param by Grouping variables. Can be provided as in
#'   [data.table::data.table()].
#' @importFrom data.table uniqueN

sdc_count <- function(DT, id_var, val_var, by = NULL) {
    substitute(
        DT[!is.na(get(val_var)),
           .(distinct_ids = data.table::uniqueN(.SD)),
           .SDcols = id_var,
           keyby = by
           ][distinct_ids < getOption("sdc.n_ids", 5L)]
    )
}

#' Internal function which creates cross-tables with number of distinct id's
#' @param DT The data.table from which the descriptives are calculated.
#' @param id_var The name of the id variable. Can be provided as bare (unquoted)
#'   column name or as a character.
#' @param val_var The name of the value variable. Can be provided as bare
#'   (unquoted) column name or as a character.
#' @param by Grouping variables. Can be provided as in
#'   [data.table::data.table()].
#'

sdc_dominance <- function(DT, id_var, val_var, by = NULL) {
    substitute(
        DT[!is.na(get(val_var)),
           #base::sum needed to avoid error with gsum
           .SD[, .(agg_val_var = base::sum(.SD)), .SDcols = val_var, by = id_var],
           keyby = by
         ][order(-agg_val_var),
           .(value_share = cumsum(agg_val_var) / sum(agg_val_var)),
           keyby = by
         ][, .SD[getOption("sdc.n_ids_dominance", 2L)], keyby = by
         ][value_share >= getOption("sdc.share_dominance", 0.85)]
    )
}
