#' @importFrom checkmate assert_data_frame assert_string assert check_class
sdc_arg_check <- function(data, id_var, val_var, by = NULL) {
    checkmate::assert_data_frame(data)
    checkmate::assert_string(id_var)
    checkmate::assert_string(val_var)
    checkmate::assert(
        combine = "or",
        .var.name = "by",
        checkmate::check_class(substitute(by), "name"),
        checkmate::check_class(substitute(by), "call"),
        checkmate::check_class(substitute(by), "character"),
    )
}

by_to_character <- function(by) {
    str <- as.character(by)
    if (length(str) == 1L) {
        return(gsub(",", ", ", str))
    }

    if (grepl("^(:|>|<|=)", str[1L])) {
        return(paste(str[2L], str[1L], str[3L]))
    } else {
        return(paste(str[-1], collapse = ", "))
    }
}
