message_options <- function() {
    message(
        "[ OPTIONS:  ",
        "sdc.n_ids: ", getOption("sdc.n_ids", 5L),
        " | sdc.n_ids_dominance: ", getOption("sdc.n_ids_dominance", 2L),
        " | sdc.share_dominance: ", getOption("sdc.share_dominance", 0.85),
        " ]"
    )
}

#' @importFrom methods hasArg
message_arguments <- function(id_var, val_var = NULL, by = NULL) {
    msg_id_var  <- paste0("id_var: ", id_var)

    if (methods::hasArg(val_var)) {
        msg_val_var <- paste0(" | val_var: ", val_var)
    } else {
        msg_val_var <- ""
    }
    if (methods::hasArg(by)) {
        by_sub <- substitute(by, env = parent.frame())
        msg_by <- paste0(" | by: ", by_to_char(by_sub))
    } else {
        msg_by <- ""
    }

    message(
        "[ SETTINGS: ",
        msg_id_var,
        msg_val_var,
        msg_by,
        " ]"
    )
}

by_to_char <- function(by) {
    str <- as.character(by)
    if (length(str) == 1L) {
        return(gsub(",", ", ", fixed = TRUE, str))
    }

    if (grepl("^(:|>|<|=)", str[1L])) {
        return(paste(str[2L], str[1L], str[3L]))
    } else {
        return(paste(str[-1], collapse = ", "))
    }
}
