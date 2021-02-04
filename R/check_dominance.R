#' Internal function which creates cross-tables with number of distinct id's
#' @inheritParams common_arguments
#' @importFrom data.table setnames
check_dominance <- function(data, id_var, val_var = NULL, by = NULL) {
  agg_val_var <- value_share <- NULL # removes NSE notes in R CMD check

  # handle the case where no val_var is provided
  if (is.null(val_var)) {
    return(data.table(value_share = NA_real_))
  }
  # browser()

  ids <- unique(data[[id_var]])


  temp_name <- by_name <- NULL

  # Additional steps if 'by' is not just plain character:
  if (!is.null(by) && !is.character(by)) {
    # Check if 'by' is a list of variable names (either as character or
    # language)
    vars_to_check <- grep(
      "^(\\.|list|c)$", as.character(by), value = TRUE, invert = TRUE
    )

    if (length(vars_to_check) > 0L) {
      if (":" %in% vars_to_check) {
        range_vars <- grep(":", vars_to_check, value = TRUE, fixed = TRUE, invert = TRUE)
        range_pos <- vapply(
          range_vars,
          function(x) which(x == names(data)),
          FUN.VALUE = integer(1L)
        )
        by <- names(data)[seq(range_pos[1], range_pos[2])]

      } else if (all(vars_to_check %in% names(data))) {

        # Case 1: 'by' is just a list of variable names
        by_char <- as.character(by)
        if (length(by_char) == 1L) {
          by <- by_char
        }
      } else {

        # Case 2: 'by' is an expression
        # Create a temporary variable which evaluates by before slicing the
        # data.
        temp_name <- paste0("tmp_", sample(seq(100000, 999999), 1L))
        by_name <- as.character(by)[1]
        while (temp_name %in% names(data)) {
          temp_name <- paste0("tmp_", sample(seq(100000, 999999), 1L))
        }
        data[, c(temp_name) := eval(by)]
        by <- temp_name
      }
    }
  }

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
  res <- DT[
    i = order(-agg_val_var),
    j = list(value_share = cumsum(agg_val_var) / sum(agg_val_var)),
    keyby = by_vars
  ][
    j = .SD[getOption("sdc.n_ids_dominance", 2L)],
    keyby = by_vars
  ][
    order(-value_share)
  ]

  if (!is.null(temp_name)) {
    data.table::setnames(res, old = temp_name, new = by_name)
  }
  return(res)

}

# dt[, .(agg_val_var = sum(abs(xpc), na.rm = TRUE)), by = list(id, cut(xpc, breaks = quantile(xpc, seq(0, 1, 0.1), na.rm = TRUE, type = 6L)))][
#   i = order(-agg_val_var),
#   j = .(value_share = cumsum(agg_val_var) / sum(agg_val_var)),
#   keyby = cut
# ][
#   j = .SD[getOption("sdc.n_ids_dominance", 2L)],
#   keyby = cut
# ]
