message_options <- function() {
    message(
        "[ OPTIONS:  ",
        "sdc.n_ids: ", getOption("sdc.n_ids", 5L),
        " | sdc.n_ids_dominance: ", getOption("sdc.n_ids_dominance", 2L),
        " | sdc.share_dominance: ", getOption("sdc.share_dominance", 0.85),
        " ]"
    )
}

message_arguments <- function(id_var, val_var = NULL, by = NULL) {
    by_sub <- by_to_char(substitute(by, env = parent.frame()))

    msg_id_var  <- paste0("id_var: ", id_var)

    msg_val_var <- ""
    if (!is.null(val_var)) {
        msg_val_var <- paste0(" | val_var: ", val_var)
    }

    msg_by <- ""
    if (!is.null(by_sub) && by_sub != "") {
            msg_by <- paste0(" | by: ", by_sub)
    }

    message(
        "[ SETTINGS: ",
        msg_id_var,
        msg_val_var,
        msg_by,
        " ]"
    )
}
