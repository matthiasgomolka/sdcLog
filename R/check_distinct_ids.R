#' Internal function which creates cross-tables with number of distinct id's
#' @param DT [data.table] from which the descriptives are calculated.
#' @param id_var [character] The name of the id variable.
#' @param val_var [character] vector of value variables on which descriptives
#'   are computed.
#' @param by Grouping variables (or expression). Can be provided as in
#'   [data.table::data.table()].
#' @importFrom data.table uniqueN

check_distinct_ids <- function(DT, id_var, val_var, by = NULL) {
  substitute(
    DT[!is.na(get(val_var)),
      .(distinct_ids = data.table::uniqueN(.SD)),
      .SDcols = id_var,
      keyby = by
    ][order(distinct_ids)]
    # [distinct_ids < getOption("sdc.n_ids", 5L)]
  )
}
