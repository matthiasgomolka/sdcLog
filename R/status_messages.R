message_options <- function() {
    message(
        "[ ",
        "sdc.n_ids: ", getOption("sdc.n_ids", 5L),
        " | sdc.n_ids_dominance: ", getOption("sdc.n_ids_dominance", 2L),
        " | sdc.share_dominance: ", getOption("sdc.share_dominance", 0.85),
        " ]"
    )
}

message_arguments <- function(id_var, val_var = NULL, by = NULL) {
    message(
        "[ ",
        "id_var: ", id_var,
        " | val_var: ", val_var,
        # if is.character(by) then print, otherwise skip
        " | by: ", paste(by, collapse = ", "),
        " ]"
    )
}


#new message arguments function
message_arguments <- function(data, id_var, val_var = NULL, by = NULL, model = NULL, n_max = NULL, n_min = NULL) {
    if (hasArg(val_var) & is.null(by) & !hasArg(model) & !hasArg(n_min) & !hasArg(n_max)) {
        message(
            "[ ",
            "data: ", deparse(substitute(data)),
            " | id_var: ", id_var,
            " | val_var: ", val_var,
            " ]"
        )
    }
    if (hasArg(val_var) & hasArg(by) & is.character(by) & !hasArg(n_min) & !hasArg(n_max)) {
        message(
            "[ ",
            "data: ", deparse(substitute(data)),
            " | id_var: ", id_var,
            " | val_var: ", val_var,
            " | by: ", paste(by, collapse = ", "),
            " ]"
        )
    }
    if (hasArg(model)) {
        message(
            "[ ",
            "data: ", deparse(substitute(data)),
            " | id_var: ", id_var,
            " | model: ", deparse(substitute(model)),
            " | model_class: ", class(model),
            " ]"
        )
    }
    if (hasArg(val_var) & is.null(by) & hasArg(n_min) & hasArg(n_max)) {
        message(
            "[ ",
            "data: ", deparse(substitute(data)),
            " | id_var: ", id_var,
            " | val_var: ", val_var,
            " | n_min: ", n_min,
            " | n_max: ", n_max,
            " ]"
        )
    }
    if (hasArg(val_var) & hasArg(by) & is.character(by) & hasArg(n_min) & hasArg(n_max)) {
        message(
            "[ ",
            "data: ", deparse(substitute(data)),
            " | id_var: ", id_var,
            " | val_var: ", val_var,
            " | by: ", paste(by, collapse = ", "),
            " | n_min: ", n_min,
            " | n_max: ", n_max,
            " ]"
        )
    }
}

