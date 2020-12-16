#' Internal function which creates cross-tables with number of distinct id's
#' @inheritParams common_arguments
check_dominance <- function(data, id_var, val_var, by = NULL) {
  substitute(
    data[
      i = !is.na(get(val_var)),
      j = .SD[, .(agg_val_var = sum(abs(get(val_var)))), by = id_var],
      keyby = by
    ][
      i = order(-agg_val_var),
      j = .(value_share = cumsum(agg_val_var) / sum(agg_val_var)),
      keyby = by
    ][
      j = .SD[getOption("sdc.n_ids_dominance", 2L)],
      keyby = by
    ][order(-value_share)]
  )
}