#' Internal function which creates cross-tables with number of distinct id's
#' @inheritParams common_arguments
check_dominance <- function(data, id_var, val_var, by = NULL) {
  substitute(
    data[!is.na(get(val_var)),
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
    ][, .SD[getOption("sdc.n_ids_dominance", 2L)], keyby = by
    ][order(-value_share)]
  )
}
