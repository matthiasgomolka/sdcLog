#' Internal function which creates cross-tables with number of distinct id's
#' @inheritParams common_arguments
#' @importFrom data.table uniqueN

check_distinct_ids <- function(data, id_var, val_var = NULL, by = NULL) {
  distinct_ids <- NULL # to silence NSE notes in RCDM check

  # handle the case where no val_var is provided
  if (is.null(val_var)) {
    val_var <- id_var
  }

  distinct_ids <- data[
    i = !is.na(get(val_var)),
    j = list(distinct_ids = data.table::uniqueN(get(id_var))),
    keyby = by
  ][
    order(distinct_ids)
  ]

  structure(
    distinct_ids,
    class = c("sdc_distinct_ids", "data.table", "data.frame")
  )
}
