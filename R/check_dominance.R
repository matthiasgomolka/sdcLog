#' Internal function which creates cross-tables with number of distinct id's
#' @inheritParams common_arguments
#' @importFrom data.table fifelse setnafill
#' @noRd
check_dominance <- function(data, id_var, val_var = NULL, by = NULL) {
  # remove NSE notes in R CMD check
  agg_val_var <- value_share <- id_na <- simple_share <- value_share_na <- NULL


  class <- c("sdc_dominance", "data.table", "data.frame")
  # handle the case where no val_var is provided
  if (is.null(val_var)) {
    return(structure(data.table(value_share = NA_real_), class = class))
  }

  # dominance <- data[
  #   j = list(agg_val_var = sum(abs(get(val_var)), na.rm = TRUE)),
  #   keyby = c(id_var, by)
  # ][
  #   i = order(-agg_val_var),
  #   j = list(value_share = cumsum(agg_val_var) / sum(agg_val_var)),
  #   keyby = by
  # ][
  #   j = .SD[getOption("sdc.n_ids_dominance", 2L)],
  #   keyby = by
  # ][
  #   order(-value_share)
  # ]

  # missing_id_var = "structural" ----
  DT <- data[
    j = list(agg_val_var = sum(abs(get(val_var)), na.rm = TRUE)),
    keyby = c(id_var, by)
  ][
    i = order(-agg_val_var),
    j = list(
      id_na = data.table::fifelse(is.na(get(id_var)), TRUE, FALSE),
      simple_share = agg_val_var / sum(agg_val_var),
      value_share = cumsum(agg_val_var) / sum(agg_val_var)
    ),
    keyby = by
  ]

  # dominance <- DT[
  #   # j = -c("id_na", "simple_share"),
  #   # with = FALSE
  # ]

  na_shares <- DT[
    i = id_na == TRUE,
    j = list(id_na, value_share_na = simple_share),
    keyby = by
  ]
  if (nrow(na_shares) > 0L) {
    # DT <- DT[na_shares, on = "simple_share"]
    DT <- merge(DT, na_shares, by = c("id_na", by), all.x = TRUE)
    data.table::setnafill(DT, type = "nocb", cols = "value_share_na")
    DT[, value_share := value_share - value_share_na, keyby = by]
    DT <- DT[id_na == FALSE]
  }

  dominance <- DT[
    j = .SD[getOption("sdc.n_ids_dominance", 2L)],
    keyby = by
  ][
    i = order(-value_share),
    j = c(by, "value_share"),
    with = FALSE
  ]

  #----

  structure(dominance, class = class)
}

# DT <- data.table::copy(data)
# dat <- DT[
#   j = list(agg_val_var = sum(abs(get(val_var)), na.rm = TRUE)),
#   keyby = c(id_var, by)
# ][
#   i = order(-agg_val_var),
#   j = list(
#     id_na = data.table::fifelse(is.na(get(id_var)), TRUE, FALSE),
#     simple_share = agg_val_var / sum(agg_val_var),
#     value_share = cumsum(agg_val_var) / sum(agg_val_var)
#   ),
#   keyby = by
# ]
# na_shares <- dat[id_na == TRUE, simple_share, keyby = by]
#
# dat[
#   j = -c("id_na", "simple_share"),
#   with = FALSE
# ][
#   na_shares
# ][
#   j = list(
#     value_share = data.table::fifelse(
#       value_share >= simple_share, value_share -  simple_share, value_share
#     )
#   ),
#   keyby = by
# ][
#   j = .SD[getOption("sdc.n_ids_dominance", 2L)],
#   keyby = by
# ][
#   order(-value_share)
# ]
