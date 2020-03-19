# pure technical settings
.datatable.aware <- TRUE

#' @importFrom checkmate assert_data_frame assert_string assert check_class
#' @importFrom methods hasArg
check_args <- function(data, id_var, val_var = NULL, by = NULL) {
    checkmate::assert_data_frame(data)
    checkmate::assert_string(id_var)
    if (!is.null(val_var)) {
        checkmate::assert_string(val_var)
    }
    if (methods::hasArg(by)) {
        checkmate::assert(
            combine = "or",
            .var.name = "by",
            checkmate::check_class(substitute(by), "name"),
            checkmate::check_class(substitute(by), "call"),
            checkmate::check_class(substitute(by), "character"),
        )
    }
}


by_to_char <- function(by) {
    str <- as.character(by)
    if (length(str) == 1L) {
        return(gsub(",", ", ", fixed = TRUE, str))
    }

    if (grepl("^(:|>|<|=)", str[1L])) {
        paste(str[2L], str[1L], str[3L])
    } else {
        paste(str[-1], collapse = ", ")
    }
}
