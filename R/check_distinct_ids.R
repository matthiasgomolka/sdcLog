#' Internal function which creates cross-tables with number of distinct id's
#' @inheritParams common_arguments
#' @importFrom data.table uniqueN

check_distinct_ids <- function(data, id_var, val_var = NULL, by = NULL) {
  # handle the case where no val_var is provided
  if (is.null(val_var)) {
    val_var <- id_var
  }
  substitute(
    data[!is.na(get(val_var)),
      .(distinct_ids = data.table::uniqueN(.SD)),
      .SDcols = id_var,
      keyby = by
    ][order(distinct_ids)]
  )
}
