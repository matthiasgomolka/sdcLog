is_dummy <- function(x) {
    any(
        is.logical(x),
        is.factor(x),
        is.character(x),
        {
            uniques <- sort(unique(x))
            ifelse(length(uniques) == 2L,
                   all.equal(uniques, c(0L, 1L)) == TRUE,
                   FALSE)
        }
    )
}
