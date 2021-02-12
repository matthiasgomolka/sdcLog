#' Internal function which creates cross-tables with number of distinct id's
#' @inheritParams common_arguments
check_dominance <- function(data, id_var, val_var = NULL, by = NULL) {
  agg_val_var <- value_share <- NULL # removes NSE notes in R CMD check

  class <- c("sdc_dominance", "data.table", "data.frame")
  # handle the case where no val_var is provided
  if (is.null(val_var)) {
    return(structure(data.table(value_share = NA_real_), class = class))
  }

  dominance <- data[
    j = list(agg_val_var = sum(abs(get(val_var)), na.rm = TRUE)),
    keyby = c(id_var, by)
  ][
    i = order(-agg_val_var),
    j = list(value_share = cumsum(agg_val_var) / sum(agg_val_var)),
    keyby = by
  ][
    j = .SD[getOption("sdc.n_ids_dominance", 2L)],
    keyby = by
  ][
    order(-value_share)
  ]

  structure(dominance, class = class)
}
