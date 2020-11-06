#' Internal function which creates cross-tables with number of distinct id's
#' @param DT [data.table] from which the descriptives are calculated.
#' @param id_var [character] The name of the id variable.
#' @param val_var [character] vector of value variables on which descriptives
#'   are computed.
#' @param by Grouping variables (or expression). Can be provided as in
#'   [data.table::data.table()].

check_dominance <- function(DT, id_var, val_var, by = NULL) {
  substitute(
    DT[!is.na(get(val_var)),
      # base::sum needed to avoid error with gsum
      .SD[,
        .(agg_val_var = base::sum(.SD)),
        .SDcols = val_var,
        by = id_var
      ],
      keyby = by
    ][order(-agg_val_var),
      .(value_share = cumsum(agg_val_var) / sum(agg_val_var)),
      keyby = by
    ][, .SD[getOption("sdc.n_ids_dominance", 2L)], keyby = by][value_share >= getOption("sdc.share_dominance", 0.85)]
  )
}