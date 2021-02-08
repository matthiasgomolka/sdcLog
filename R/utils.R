# pure technical settings
.datatable.aware <- TRUE

#' @importFrom checkmate assert_data_frame assert_string assert_subset
check_args <- function(
  data, id_var, val_var = NULL, by = NULL, zero_as_NA = NULL
) {
  checkmate::assert_data_frame(data)
  col_names <- names(data)

  checkmate::assert_string(id_var)
  checkmate::assert_subset(id_var, choices = names(data))

  checkmate::assert_string(val_var, null.ok = TRUE)

  checkmate::assert_character(by, any.missing = FALSE, null.ok = TRUE)
  checkmate::assert_subset(by, choices = names(data))

  checkmate::assert_logical(zero_as_NA, len = 1L, null.ok = TRUE)
}


by_to_char <- function(by) {
  str <- as.character(by)
  if (length(str) == 1L) {
    return(gsub(",", ", ", fixed = TRUE, str))
  }

  # not sure anymore, what the purpose of the following code is ...
  if (grepl("^(:|>|<|=)", str[1L])) {
    paste(str[2L], str[1L], str[3L])
  } else {
    paste(str[-1], collapse = ", ")
  }
}
