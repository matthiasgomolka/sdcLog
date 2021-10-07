#' Internal function which creates cross-tables with number of distinct id's
#' @inheritParams common_arguments
#' @importFrom data.table fifelse setnafill setnames setorderv
#' @noRd
check_dominance <- function(data, id_var, val_var = NULL, by = NULL) {
  # remove NSE notes in R CMD check
  agg_val_var <- value_share <- id_na <- cum_value_share <- value_share_na <-
    NULL


  class <- c("sdc_dominance", "data.table", "data.frame")
  # handle the case where no val_var is provided
  if (is.null(val_var)) {
    return(
      structure(data.table::data.table(value_share = NA_real_), class = class)
    )
  }

  # missing_id_var = "structural" ----
  # distinguish between NA and non-NA id's and calculate the value_share for
  # each id and the cumulative value_share
  dt <- data[
    j = list(agg_val_var = sum(abs(get(val_var)), na.rm = TRUE)),
    keyby = c(id_var, by)
  ][
    i = order(-agg_val_var),
    j = list(
      id_na = data.table::fifelse(is.na(get(id_var)), TRUE, FALSE),
      value_share = agg_val_var / sum(agg_val_var),
      cum_value_share = cumsum(agg_val_var) / sum(agg_val_var)
    ),
    keyby = by
  ]

  # calculate the value share of NA id's in order to subtract it later
  na_shares <- dt[id_na == TRUE, list(value_share_na = value_share), keyby = by]

  if (nrow(na_shares) > 0L) {
    # The following code may look unnecessarily complicated, but it's necessary
    # in order to handle by groups correctly.
    # We first bind / merge the value_share_na, ...
    if (is.null(by)) {
      dt <- cbind(dt, na_shares)
    } else {
      dt <- merge(dt, na_shares, by = by, all.x = TRUE, sort = FALSE)
    }
    # ... then we subtract the share of NA from the cumulative value share.
    dt[, cum_value_share := cum_value_share - value_share_na, keyby = by]
    # Lastly, we delete rows where the id is NA.
    dt <- dt[id_na == FALSE]
  }

  if (nrow(dt) == 0L) { # handle the edge case with no ID's
    cols_to_keep <- setdiff(
      names(dt),
      c("id_na", "value_share", "value_share_na")
    )
    dominance <- dt[, cols_to_keep, with = FALSE]

  } else { # general case
    dominance <- dt[
      j = .SD[min(getOption("sdc.n_ids_dominance", 2L), .N)],
      # min() necessary to handle the edge case with only a single ID
      keyby = by,
      .SDcols = "cum_value_share"
    ]
  }
  data.table::setnames(dominance, old = "cum_value_share", new = "value_share")
  data.table::setorderv(dominance, "value_share", order = -1L)

  structure(dominance, class = class)
}
