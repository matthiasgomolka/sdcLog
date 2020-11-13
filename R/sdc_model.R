#' Disclosure control for models
#' @description Checks if your model complies to RDC rules. Checks for overall
#'   number of entities and number of entities for each level of dummy
#'   variables.
#' @inheritParams common_arguments
#' @importFrom data.table as.data.table uniqueN rbindlist setDT := %flike%
#' @importFrom broom augment tidy
#' @importFrom stats na.omit
#' @importFrom checkmate assert_data_frame assert_string
#' @export
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
#' @return A [list] of class `sdc_model` with detailed information about
#'   options, settings, and compliance with the distinct entities criterion.
sdc_model <- function(data, model, id_var) {
  var_level <- var <- level <- term <- NULL # to silence NSE notes in RCDM check

  # check inputs
  check_args(data, id_var)

  data.table::setDT(data)

  # model df
  data_model <- tryCatch(
    broom::augment(model),
    error = function(error) {
      paste0(
        "Models of class '", class(model), "' are not yet supported."
      )
    }
  )

  model_vars <- setdiff(
    names(data_model),
    c(
      ".fitted", ".se.fit", ".resid", ".hat", ".sigma", ".cooksd",
      ".std.resid", ".rownames", ".cluster"
    )
  )

  model_df <- data[, c(id_var, model_vars), with = FALSE]
  model_df <- stats::na.omit(model_df)

  # general check for number of distinct ID's ----
  # no call of check_distinct_ids() because we have no single val_var here
  distinct_ids <-
    model_df[, list(distinct_ids = data.table::uniqueN(get(id_var)))]#

  # warning via print method for distinct ID's
  class(distinct_ids) <- c("sdc_distinct_ids", class(distinct_ids))
  if (nrow(distinct_ids[distinct_ids < getOption("sdc.n_ids", 5L)]) > 0L) {
    warning(
      crayon::bold("Potential disclosure problem: "),
      "Not enough distinct entities", ".",
      call. = FALSE
    )
  }


  # dummy cols ----
  var_df <- model_df[, model_vars, with = FALSE]

  dummy_vars <- vapply(var_df, is_dummy, FUN.VALUE = logical(1L))
  dummy_vars <- names(dummy_vars)[dummy_vars == TRUE]
  names(dummy_vars) <- dummy_vars

  dummy_data <- model_df[, c(id_var, dummy_vars), with = FALSE]

  # warnings for dummy variables
  dummy_list <- lapply(dummy_vars, function(x) {
    distinct_ids_level <- eval(
      check_distinct_ids(dummy_data, id_var, val_var = x, by = x)
    )
    class(distinct_ids_level) <-
      c("sdc_distinct_ids", class(distinct_ids_level))
    distinct_ids_level
  })

  list_warning(dummy_list)


  # interactions ----
  dummy_levels <- lapply(dummy_vars, function(x) {
    DT <- data.table::CJ(var = x, level = unique(as.character(data[[x]])))
    DT[, var_level := paste0(var, level)]
  })
  dummy_levels <- data.table::rbindlist(dummy_levels)

  interactions <- data.table::setDT(broom::tidy(model))
  interactions <- interactions[term %flike% ":", term]
  interactions <- strsplit(interactions, ":", fixed = TRUE)
  interactions <- lapply(interactions, function(x) {
    # unlist() drops all non-dummy variables. This is fine, because possible
    # problems would be caught during the simple distinct id checks for dummy
    # variables
    unlist(
      lapply(x, function(y) dummy_levels[var_level == y, var])
    )
  })

  dummy_interactions <-
    !(vapply(interactions, length, FUN.VALUE = integer(1L)) == 1L)
  interactions <- unique(interactions[dummy_interactions])
  names(interactions) <- vapply(
    interactions, paste0, collapse = ":", FUN.VALUE = character(1L)
  )

  inter_list <- mapply(
    interactions, names(interactions),
    SIMPLIFY = FALSE,
    FUN = function(x, name_x) {
      inter_df <- data.table::copy(model_df)
      inter_df[, (name_x) := do.call(paste, args = c(.SD, sep = ":")),
               .SDcols = x]

      structure(
        eval(check_distinct_ids(
          inter_df, id_var, val_var = name_x, by = name_x
        )),
        class = c("sdc_distinct_ids", "data.table", "data.frame")
      )
    })

  list_warning(inter_list)

  # return list with all problem df's &| messages
  structure(
    list(
      message_options = message_options(),
      message_arguments = message_arguments(id_var = id_var),
      distinct_ids = distinct_ids,
      dummies = dummy_list,
      interactions = inter_list
    ),
    class = c("sdc_model", "list")
  )
}


conditional_print <- function(list) {
  distinct_ids <- NULL # removes NSE notes in R CMD check
  problems <- vapply(
    list,
    function(x) nrow(x[distinct_ids < getOption("sdc.n_ids", 5L)]) > 0L,
    FUN.VALUE = logical(1L)
  )
  for (i in seq_along(problems)) {
    if (problems[[i]] | getOption("sdc.info_level", 1L) > 1L) {
      print(list[i])
    }
  }
}


list_warning <- function(list) {
  distinct_ids <- NULL # removes NSE notes in R CMD check
  problems <- vapply(
    list,
    function(x) nrow(x[distinct_ids < getOption("sdc.n_ids", 5L)]) > 0L,
    FUN.VALUE = logical(1L)
  )


  if (sum(problems) > 0L) {
    warning(
      crayon::bold("Potential disclosure problem: "),
      "Not enough distinct entities", ".",
      call. = FALSE
    )
  }
}
