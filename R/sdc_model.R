#' check if your model complies to RDSC rules
#' @param data The dataset (anything which can be coerced to data.table) from
#'   which the model is estimated.
#' @param model The estimated model object. Can be a model type like lm, glm and
#'   various others (anything which can be handled by [`broom::augment()`]).
#' @param id_var The name of the id variable as a character.
#' @importFrom data.table as.data.table uniqueN
#' @importFrom broom augment
#' @importFrom stats na.omit
#' @importFrom checkmate assert_data_frame assert_string
#' @export

sdc_model <- function(data, model, id_var) {

    # check inputs
    checkmate::assert_data_frame(data)
    checkmate::assert_string(id_var)

    # status messages
    message_options()
    message_arguments(id_var)

    data <- data.table::as.data.table(data)

    #model df
    data_model <- broom::augment(model)

    model_vars <- setdiff(
        names(data_model),
        c(".fitted", ".se.fit", ".resid", ".hat", ".sigma", ".cooksd",
          ".std.resid", ".rownames")
    )

    model_df <- data[, c(id_var, model_vars), with = FALSE]
    model_df <- stats::na.omit(model_df)

    # general check for number of distinct ID's
    distinct_ids <- data.table::uniqueN(model_df[[id_var]])

    #warning via print method for distinct ID's
    distinct_ids <- data.table::as.data.table(distinct_ids)
    distinct_ids <- distinct_ids[distinct_ids < getOption("sdc.n_ids", 5L)]
    class(distinct_ids) <- c("sdc_counts", class(distinct_ids))
    print(distinct_ids)

    #extract dummy cols
    var_df <- model_df[, model_vars, with = FALSE]

    dummy_vars <- vapply(var_df, sdc_find_dummy_cols, FUN.VALUE = logical(1))
    dummy_vars <- names(dummy_vars)[dummy_vars == TRUE]


    #select df for warning dominance
    model_var_no_dummy <- setdiff(model_vars, dummy_vars)
    model_df_no_dummy <- model_df[, c(id_var, model_var_no_dummy), with = FALSE]


    #warning dominance with print method
    dominance_list <- lapply(seq_along(model_var_no_dummy), function(x){
        dominance <-
            eval(sdc_dominance(model_df_no_dummy, id_var, model_var_no_dummy))
        class(dominance) <- c("sdc_dominance", class(dominance))
        dominance
    })

    names(dominance_list) <- model_var_no_dummy
    print(dominance_list)


    # return early if no dummy cols exist
    if (length(dummy_vars) == 0) {
        message("No dummy-variables in data")
        invisible(return(TRUE))
    }

    dummy_data <- model_df[, c(id_var, dummy_vars), with = FALSE]

    # warnings for dummy variables
    dummy_list <- lapply(dummy_vars, function(x) {
        distinct_ids_per_value <-
            eval(sdc_count(dummy_data, id_var, val_var = x, by = x))
        class(distinct_ids_per_value) <- c(
            "sdc_counts",
            class(distinct_ids_per_value)
        )
        distinct_ids_per_value
    })

    names(dummy_list) <- dummy_vars
    print(dummy_list)

    # return list with all problem df's &| messages
    invisible(
        list(
            distinct_ids = distinct_ids,
            dominance_list = dominance_list,
            dummy_list = dummy_list
        )
    )
}
