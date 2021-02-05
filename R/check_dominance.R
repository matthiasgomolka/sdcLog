#' Internal function which creates cross-tables with number of distinct id's
#' @inheritParams common_arguments
#' @importFrom data.table setnames
check_dominance <- function(data, id_var, val_var = NULL, by = NULL) {
  agg_val_var <- value_share <- NULL # removes NSE notes in R CMD check

  # handle the case where no val_var is provided
  if (is.null(val_var)) {
    return(data.table(value_share = NA_real_))
  }

  ids <- unique(data[[id_var]])

  id_slices <- lapply(
    ids,
    function(x) {
      filter <- which(data[[id_var]] == x)
      eval(substitute(
        data[
          i = filter,
          j = list(agg_val_var = sum(abs(get(val_var)), na.rm = TRUE)),
          keyby = by
        ]
      ))
    })
  DT <- rbindlist(id_slices, idcol = id_var)

  by_vars <- setdiff(names(DT), c(id_var, "agg_val_var"))
  DT[
    i = order(-agg_val_var),
    j = list(value_share = cumsum(agg_val_var) / sum(agg_val_var)),
    keyby = by_vars
  ][
    j = .SD[getOption("sdc.n_ids_dominance", 2L)],
    keyby = by_vars
  ][
    order(-value_share)
  ]
}
