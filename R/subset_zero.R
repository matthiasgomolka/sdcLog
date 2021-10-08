# Helper function to remove uncritical rows from a data.table
#
# This is necessary in the case where a continuous variable was transformed into
# a dichotomous variable for SDC purposes. Then, is may only contain the values
# "<zero>" and "<non-zero>". It is uncritical if there are only a few "<zero>"
# values, since the variable used to be continuous. This function removes these
# rows from a data.table.
#
# This is necessary for preventing wrong disclosure control warnings.
#
#' @noRd
subset_zero <- function(dt, var_names) {
    if (identical(var_names, character())) return(dt)

    for (var in var_names) {
        if (isTRUE(attr(dt[[var]], "was_continuous"))) {
            dt <- subset(dt[get(var) != "<zero>"])
        }
    }
    return(dt)
}
