#' Internal function which creates cross-tables with number of distinct id's
#' @inheritParams common_arguments
#' @importFrom data.table uniqueN

check_distinct_ids <- function(data, id_var, val_var, by = NULL) {
  substitute(
    data[!is.na(get(val_var)),
      .(distinct_ids = data.table::uniqueN(.SD)),
      .SDcols = id_var,
      keyby = by
    ][order(distinct_ids)]
  )
}
