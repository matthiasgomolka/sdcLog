#' Check if your descriptive statistics comply to statistical disclosure control
#' @param data [data.frame] from which the descriptives are calculated.
#' @param id_var [character] The name of the id variable.
#' @param val_var [character] vector of value variables on which descriptives
#'   are computed.
#' @param by Grouping variables (or expression). Can be provided as in
#'   [data.table::data.table()].
#' @param NA_vals [numeric] Value(s) to be recognized as NA's.
#' @importFrom data.table as.data.table
#' @export

sdc_descriptives <- function(data, id_var, val_var, by = NULL, NA_vals = NULL) {
    # input checks
    check_args(data, id_var, val_var, by, NA_vals)

    # status messages
    # message_options()
    # message_arguments(id_var, val_var, by)
    #message_options <- capture_messages(message_options())
    #message_arguments <- capture_messages(message_arguments(id_var, val_var, by))

    data <- data.table::as.data.table(data)

    # handling 0's/other NA's
    if (!is.null(NA_vals)) {
        eval(substitute(data[get(val_var) %in% NA_vals, val_var := NA_real_]))
    }

    # best guess NA's
    possible_na_df <- data[ , `:=`(count = .N) , by = val_var][which.max(count)
                        ][, .(possible_na = get(val_var),
            value_share = count/length(data[[val_var]]))
            ][value_share >= getOption("sdc.share_possible_na", 0.25)]

    if (nrow(possible_na_df) > 0){
        message("The value '",possible_na_df[[1,1]],"' occurs frequently in the data: Is it used as coding for NA?")}


    # check distinct_ids
    expr_distinct_ids <- eval(substitute(
        check_distinct_ids(data, id_var, val_var, by)
    ))
    distinct_ids <- eval(expr_distinct_ids)
    class(distinct_ids) <- c("sdc_distinct_ids", class(distinct_ids))

    # print(distinct_ids)
    if (nrow(distinct_ids) > 0L) {
        warning(
            crayon::bold("Potential disclosure problem: "),
            "Not enough distinct entities", ".",
            call. = FALSE
        )
    }

    # check dominance
    expr_dominance <- eval(substitute(
        check_dominance(data, id_var, val_var, by)
    ))
    dominance <- eval(expr_dominance)
    class(dominance) <- c("sdc_dominance", class(dominance))

    # print(dominance)
    if (nrow(dominance) > 0L) {
        warning(
            crayon::bold("Potential disclosure problem: "),
            "Dominant entities", ".",
            call. = FALSE
        )
    }

    res <- list(message_options = message_options(),
                message_arguments = message_arguments(id_var, val_var, by, NA_vals),
                distinct_ids = distinct_ids,
                dominance = dominance)
    class(res) <- c("sdc_descriptives", class(res))
    res
}
