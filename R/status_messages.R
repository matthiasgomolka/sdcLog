list_options <- function() {
  structure(
    list(
      sdc.n_ids = getOption("sdc.n_ids", 5L),
      sdc.n_ids_dominance = getOption("sdc.n_ids_dominance", 2L),
      sdc.share_dominance = getOption("sdc.share_dominance", 0.85)
    ),
    class = "sdc_options"
  )
}


list_arguments <- function(
  id_var, val_var = NULL, by = NULL, zero_as_NA = NULL, fill_id_var = NULL
) {
    if (isTRUE(fill_id_var)) {
        id_var <- paste(id_var, "(filled)")
    }
  structure(
    list(
      id_var = id_var,
      val_var = val_var,
      by = by,
      zero_as_NA = zero_as_NA
    ),
    class = "sdc_settings"
  )
}
