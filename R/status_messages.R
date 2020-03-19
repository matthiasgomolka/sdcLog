message_options <- function() {
    message(
        "[ OPTIONS:  ",
        "sdc.n_ids: ", getOption("sdc.n_ids", 5L),
        " | sdc.n_ids_dominance: ", getOption("sdc.n_ids_dominance", 2L),
        " | sdc.share_dominance: ", getOption("sdc.share_dominance", 0.85),
        " ]"
    )
}

message_arguments <- function(...) {
    args <- list(...)
    msg_id_var  <- paste0("id_var: ", args[["id_var"]])

    msg_val_var <- ""
    if (methods::hasArg("val_var")) {
        msg_val_var <- paste0(" | val_var: ", args[["val_var"]])
    }

    msg_by <- ""
    if (methods::hasArg(by)) {
        if (!is.null(args[["by"]])) {
            if (args[["by"]] != "") {
                msg_by <- paste0(" | by: ", args[["by"]])
            }
        }
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
        paste(str[2L], str[1L], str[3L])
    } else {
        paste(str[-1], collapse = ", ")
    }
}
