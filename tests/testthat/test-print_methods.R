library(data.table)


# test print.sdc_counts ----
context("print.sdc_counts")
counts_1 <- data.table(distinct_ids = integer(0L))

test_that("print.sdc_counts works for most simple case", {
    options(sdc.info_level = 0L)
    expect_silent(sdcLog:::print.sdc_counts(counts_1))

    options(sdc.info_level = 1L)
    expect_silent(sdcLog:::print.sdc_counts(counts_1))

    options(sdc.info_level = 2L)
    expect_message(
        sdcLog:::print.sdc_counts(counts_1),
        "No problem with number of distinct entities.",
        fixed = TRUE
    )
})

counts_2 <- data.table(distinct_ids = 3L)
expect_print.sdc_counts_2 <- function(x) {
    expect_warning(
        expect_output(
            expect_identical(x, counts_2),
            "Not enough distinct entities:\n",
            fixed = TRUE
        ),
        "Potential disclosure problem: Not enough distinct entities.",
        fixed = TRUE
    )
}

test_that("print.sdc_counts works for problematic case", {
    options(sdc.info_level = 0L)
    expect_print.sdc_counts_2(sdcLog:::print.sdc_counts(counts_2))

    options(sdc.info_level = 1L)
    expect_print.sdc_counts_2(sdcLog:::print.sdc_counts(counts_2))

    options(sdc.info_level = 2L)
    expect_print.sdc_counts_2(sdcLog:::print.sdc_counts(counts_2))
})

counts_3 <- data.table(sector = paste0("S", 1:2), distinct_ids = 3L:4L)
expect_print.sdc_counts_3 <- function(x) {
    expect_warning(
        expect_output(
            expect_identical(x, counts_3),
            "Not enough distinct entities:\n",
            fixed = TRUE
        ),
        "Potential disclosure problem: Not enough distinct entities.",
        fixed = TRUE
    )
}

test_that("print.sdc_counts works for problematic by case", {
    options(sdc.info_level = 0L)
    expect_print.sdc_counts_3(sdcLog:::print.sdc_counts(counts_3))

    options(sdc.info_level = 1L)
    expect_print.sdc_counts_3(sdcLog:::print.sdc_counts(counts_3))

    options(sdc.info_level = 2L)
    expect_print.sdc_counts_3(sdcLog:::print.sdc_counts(counts_3))
})


# test print.sdc_dominance ----
context("print.sdc_dominance")

dominance_1 <- data.table(value_share = numeric(0))

test_that("print.sdc_dominance works for most simple case", {
    options(sdc.info_level = 0L)
    expect_silent(sdcLog:::print.sdc_dominance(dominance_1))

    options(sdc.info_level = 1L)
    expect_silent(sdcLog:::print.sdc_dominance(dominance_1))

    options(sdc.info_level = 2L)
    expect_message(
        sdcLog:::print.sdc_dominance(dominance_1),
        "No problem with dominance.",
        fixed = TRUE
    )
})

dominance_2 <- data.table(value_share = 0.9)
expect_print.sdc_dominance_2 <- function(x) {
    expect_warning(
        expect_output(
            expect_identical(x, dominance_2),
            "Dominant entities:\n",
            fixed = TRUE
        ),
        "Potential disclosure problem: Dominant entities.",
        fixed = TRUE
    )
}

test_that("print.sdc_dominance works for problematic case", {
    options(sdc.info_level = 0L)
    expect_print.sdc_dominance_2(sdcLog:::print.sdc_dominance(dominance_2))

    options(sdc.info_level = 1L)
    expect_print.sdc_dominance_2(sdcLog:::print.sdc_dominance(dominance_2))

    options(sdc.info_level = 2L)
    expect_print.sdc_dominance_2(sdcLog:::print.sdc_dominance(dominance_2))
})

dominance_3 <- data.table(sector = paste0("S", 1:2), value_share = c(0.88, 0.9))
expect_print.sdc_dominance_3 <- function(x) {
    expect_warning(
        expect_output(
            expect_identical(x, dominance_3),
            "Dominant entities:\n",
            fixed = TRUE
        ),
        "Potential disclosure problem: Dominant entities.",
        fixed = TRUE
    )
}

test_that("print.sdc_dominance works for problematic by case", {
    options(sdc.info_level = 0L)
    expect_print.sdc_dominance_3(sdcLog:::print.sdc_dominance(dominance_3))

    options(sdc.info_level = 1L)
    expect_print.sdc_dominance_3(sdcLog:::print.sdc_dominance(dominance_3))

    options(sdc.info_level = 2L)
    expect_print.sdc_dominance_3(sdcLog:::print.sdc_dominance(dominance_3))
})


# test print.sdc_descriptives ----
context("print.sdc_descriptives")

#test print.sdc_model ----
context("print.sdc_model")
