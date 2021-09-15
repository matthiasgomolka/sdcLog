#' Disclosure control for models
#' @description Checks if your model complies to RDC rules. Checks for overall
#'   number of entities and number of entities for each level of dummy
#'   variables.
#' @inheritParams common_arguments
#' @param data [data.frame] which was used to build the model.
#' @importFrom data.table is.data.table as.data.table fsetequal rbindlist :=
#'   %flike% set
#' @importFrom broom augment tidy
#' @importFrom stats model.frame na.omit
#' @importFrom checkmate assert_data_frame assert_string
#' @return A [list] of class `sdc_model` with detailed information about
#'   options, settings, and compliance with the distinct entities criterion.
#' @examples
#' # Check simple models
#' model_1 <- lm(y ~ x_1 + x_2, data = sdc_model_DT)
#' sdc_model(data = sdc_model_DT, model = model_1, id_var = "id")
#'
#' model_2 <- lm(y ~ x_1 + x_2 + x_3, data = sdc_model_DT)
#' sdc_model(data = sdc_model_DT, model = model_2, id_var = "id")
#'
#' model_3 <- lm(y ~ x_1 + x_2 + dummy_3, data = sdc_model_DT)
#' sdc_model(data = sdc_model_DT, model = model_3, id_var = "id")
#' @export
sdc_model <- function(data, model, id_var = getOption("sdc.id_var")) {
  var_level <- var <- level <- NULL # to silence NSE notes in RCDM check

  # check inputs
  # input checks ----
  checkmate::assert_data_frame(data)
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  col_names <- names(data)

  checkmate::assert_string(id_var)
  checkmate::assert_subset(id_var, choices = col_names)

  checkmate::assert_true(exists("model"))

  # Extract model_frame (if possible) to check if 'data' is the same data which
  # was used in 'model'. This check is only meaningful if the model_frame can
  # actually be extracted using model.frame().
  model_frame <- tryCatch(
    data.table::as.data.table(model.frame(model)),
    error = function(error) data
  )
  if (!data.table::fsetequal(
    stats::na.omit(model_frame),
    stats::na.omit(data[, names(model_frame), with = FALSE])
  )) {
    stop("'data' is not the data.frame which was used in 'model'.")
  }


  # check if model is supported by broom::augment() ... ----
  data_model <- tryCatch(
    broom::augment(model, data = model_frame),
    error = function(error) {
      paste("Models of class'", class(model), "'are not (yet) supported.")
    }
  )

  # ... and extract the model_frame / model_dt
  model_vars <- setdiff(
    names(data_model),
    c(
      ".fitted", ".se.fit", ".resid", ".hat", ".sigma", ".cooksd", ".std.resid",
      ".rownames", ".cluster",
      id_var
      # exclude id_var because it will will be added below anyway. It might be
      # listed here, if it's used for clustering in felm() for example.
    )
  )
  names(model_vars) <- model_vars

  model_dt <- stats::na.omit(data[, c(id_var, model_vars), with = FALSE])


  # general check for number of distinct ID's ----
  distinct_ids <- structure(
    check_distinct_ids(model_dt, id_var),
    class = c("sdc_distinct_ids", "data.table", "data.frame")
  )
  # warning about too few distinct ID's are gathered and thrown below

  # column preprocessing ----
  is_dummy <- vapply(
    model_dt[, !id_var, with = FALSE],
    is_dummy,
    FUN.VALUE = logical(1L)
  )
  dummy_vars <- names(is_dummy)[is_dummy]
  names(dummy_vars) <- dummy_vars

  other_vars <- setdiff(model_vars, dummy_vars)
  names(other_vars) <- other_vars

  # Set all non-zero values to "<non-zero>" and all zero values to "<zero>".
  # This makes it possible to check the number of distinct id's for continuous
  # variables.
  for (var in other_vars) {
    data.table::set(
      model_dt,
      j = var,
      value = data.table::fifelse(model_dt[[var]] == 0L, "<zero>", "<non-zero>")
    )
    attr(model_dt[[var]], "was_continuous") <- TRUE
  }


  # create data.table of possible variable level combinations which is needed to
  # find the correct variable name corresponding to a level combination / term
  # from the model.
  var_levels <- lapply(model_vars, function(x) {
    dt <- data.table::CJ(var = x, level = unique(as.character(model_dt[[x]])))
    dt[, var_level := paste0(var, level)]
  })
  var_levels <- data.table::rbindlist(var_levels)


  # find terms from the model
  terms <- broom::tidy(model)[["term"]]
  terms_split <- strsplit(terms, split = ":", fixed = TRUE)

  # find corresponding variables to terms
  term_vars <- lapply(terms_split, function(x) {
    # unlist() drops all non-dummy variables. This is fine, because possible
    # problems would be caught during the simple distinct id checks for dummy
    # variables
    unlist(lapply(x, function(y) {
      unique(
        var_levels[var_level %in% paste0(y, c("", "<zero>", "<non-zero>")), var]
      )
    }))
  })

  # drop empty items
  term_vars <- unique(term_vars[lengths(term_vars) > 0L])
  names(term_vars) <- vapply(
    term_vars, paste0, collapse = ":", FUN.VALUE = character(1L)
  )

  # actual tests for distinct ID's for plainvariables and interactions ----
  interactions <- term_vars[grep(":", fixed = TRUE, names(term_vars))]

  plain_vars <- setdiff(term_vars, interactions)
  names(plain_vars) <- plain_vars

  plain_var_list <- lapply(plain_vars, function(x) {
    structure(
      check_distinct_ids(model_dt, id_var, val_var = x, by = x),
      class = c("sdc_distinct_ids", "data.table", "data.frame")
    )
  })

  interactions_list <- mapply(
    interactions, names(interactions),
    SIMPLIFY = FALSE,
    FUN = function(x, name_x) {
      inter_df <- data.table::copy(model_dt)
      inter_df[
        j = (name_x) := do.call(paste, args = c(.SD, sep = ":")),
        .SDcols = x
      ]

      structure(
        check_distinct_ids(inter_df, id_var, val_var = name_x, by = name_x),
        class = c("sdc_distinct_ids", "data.table", "data.frame")
      )
    })

  # combine distinct ID results from plain vars and interactions (and warn if
  # necessary)
  term_list <- c(plain_var_list, interactions_list)
  warn_distinct_ids(append(list(distinct_ids = distinct_ids), term_list))


  # return list with all messages and results
  structure(
    list(
      options = list_options(),
      settings = list_arguments(id_var = id_var),
      distinct_ids = distinct_ids,
      terms = term_list
    ),
    class = c("sdc_model", "list")
  )
}


conditional_print <- function(list) {
  distinct_ids <- NULL # removes NSE notes in R CMD check

  problems <- vapply(
    list,
    function(x) {
      var_names <- setdiff(names(x), "distinct_ids")
      x_non_zero <- subset_zero(x, var_names)
      nrow(x_non_zero[distinct_ids < getOption("sdc.n_ids", 5L)]) > 0L
    },
    FUN.VALUE = logical(1L)
  )
  for (i in seq_along(problems)) {
    if (problems[[i]] | getOption("sdc.info_level", 1L) > 1L) {
      print(list[i])
    }
  }
}
