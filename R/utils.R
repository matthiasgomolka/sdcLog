#' @importFrom data.table set
fill_na <- function(data, id_var, rows) {
    data.table::set(
        data,
        i = rows,
        j = id_var,
        value = paste0("<filled_", seq_along(rows), ">")
    )
}
