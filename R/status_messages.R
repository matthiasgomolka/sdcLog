message_options <- function() {
  c(
    "[ OPTIONS:  ",
    "sdc.n_ids: ", getOption("sdc.n_ids", 5L),
    " | sdc.n_ids_dominance: ", getOption("sdc.n_ids_dominance", 2L),
    " | sdc.share_dominance: ", getOption("sdc.share_dominance", 0.85),
    " ]"
  )
}


message_arguments <- function(
                              id_var, val_var = NULL, by = NULL, zero_as_NA = NULL) {
  msg_id_var <- paste0("id_var: ", id_var)

  msg_val_var <- ""
  if (!is.null(val_var)) {
    msg_val_var <- paste0(" | val_var: ", val_var)
  }

  msg_by <- ""
  by_null <- tryCatch(is.null(by), error = function(error) FALSE)
  if (!by_null) {
    by_char <- by_to_char(substitute(by, env = parent.frame()))
    msg_by <- paste0(" | by: ", by_char)
  }

  msg_zero_as_NA <- ""
  if (!is.null(zero_as_NA)) {
    str <- as.character(zero_as_NA)
    zero_as_NA <- paste(str, collapse = ", ")
    msg_zero_as_NA <- paste0(" | zero_as_NA: ", zero_as_NA)
  }

  c(
    "[ SETTINGS: ",
    msg_id_var,
    msg_val_var,
    msg_by,
    msg_zero_as_NA,
    " ]"
  )
}
