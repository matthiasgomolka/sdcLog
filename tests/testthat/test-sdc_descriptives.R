# test setup ####
library(data.table)

set.seed(1)
n <- 20L
test_dt <- data.table(
    id = rep_len(LETTERS[1L:10L], n),
    year = sort(rep_len(2019L:2020L, n)),
    val = runif(n, min = 1, max = 10),
    key = "id"
)
test_dt[, sector := sort(rep_len(paste0("S", 1L:2L), n))]
test_dt[id == "A" & year == 2019L, val := NA_real_]
test_dt[id %chin% c("A", "F") & year == 2020L, val := val * 50]
setcolorder(test_dt, c("id", "sector", "year"))

# test sdc_count ----
context("sdc_count")
test_that("sdc_count() returns a call", {
    expect_true(is.call(sdc_count(test_dt, "id", "val")))
    expect_type(sdc_count(test_dt, "id", "val"), "language")
})

## functionality tests
counts_ref_1 <- data.table(distinct_ids = integer())
counts_ref_2 <- data.table(sector = character(), distinct_ids = integer(),
                           key = "sector")
counts_ref_3 <- data.table(sector = "S1", year = 2019L, distinct_ids = 4L,
                           key = c("sector", "year"))
test_that("sdc_count() counts correctly", {
    expect_identical(
        eval(sdc_count(test_dt, "id", "val")),
        counts_ref_1
    )
    expect_identical(
        eval(sdc_count(test_dt, "id", "val", by = sector)),
        counts_ref_2
    )
    expect_identical(
        eval(sdc_count(test_dt, "id", "val", by = .(sector, year))),
        counts_ref_3
    )
})

# test sdc_dominance ----
context("sdc_dominance")
## pure technical tests
test_that("sdc_count() returns a call", {
    expect_true(is.call(sdc_dominance(test_dt, "id", "val")))
    expect_type(sdc_dominance(test_dt, "id", "val"), "language")
})

## functionality tests
dominance_ref_1 <- data.table(value_share = numeric())
dominance_ref_2 <- data.table(sector = "S2", value_share = 0.888866740023071,
                              key = "sector")
dominance_ref_3 <- data.table(
    sector = c("S1", "S2"), year = rep(2020L, 2L),
    value_share = c(0.913682312146633, 0.934568784234764),
    key = c("sector", "year")
)

test_that("sdc_dominance() calculates correctly", {
    expect_identical(
        eval(sdc_dominance(test_dt, "id", "val")),
        dominance_ref_1
    )
    expect_equal(
        eval(sdc_dominance(test_dt, "id", "val", by = sector)),
        dominance_ref_2
    )
    expect_equal(
        eval(sdc_dominance(test_dt, "id", "val", by = .(sector, year))),
        dominance_ref_3
    )
})


# test sdc_descriptives ####
context("sdc_descriptives")
## pure technical tests
std_opts <- list(sdc.n_ids = 5L,
                 sdc.n_ids_dominance = 2L,
                 sdc.share_dominance = 0.85)

test_that("sdc_descriptices works in simple cases", {

    # descriptives setup 1 ####
    class(counts_ref_1)    <- c("sdc_counts"   , class(counts_ref_1))
    class(dominance_ref_1) <- c("sdc_dominance", class(dominance_ref_1))
    descriptives_ref_1 <- list(counts = counts_ref_1,
                               dominance = dominance_ref_1,
                               options = std_opts)

    descriptices_expect_1 <- function(x) {
        messages <- capture_messages(expect_identical(x, descriptives_ref_1))
        expect_match(
            paste0(messages, collapse = ""),
            paste0("[ sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85 ]\n",
                   "No problem with number of distinct entities.\n",
                   "No problem with dominance.",
                   collapse = ""),
            fixed = TRUE
        )
    }

    # descriptives tests 1 ####
    descriptices_expect_1(
        sdc_descriptives(test_dt, "id", "val")
    )
    descriptices_expect_1(
        sdc_descriptives(test_dt, id, val)
    )
    descriptices_expect_1(
        sdc_descriptives(test_dt, "id", val)
    )
    descriptices_expect_1(
        sdc_descriptives(test_dt, id, "val")
    )
})

test_that("sdc_descriptices works in medium cases", {
    # descriptives setup 2 ####
    class(counts_ref_2)    <- c("sdc_counts"   , class(counts_ref_2))
    class(dominance_ref_2) <- c("sdc_dominance", class(dominance_ref_2))
    descriptives_ref_2 <- list(counts = counts_ref_2,
                               dominance = dominance_ref_2,
                               options = std_opts)

    descriptives_expect_2 <- function(x) {
        expect_output(
            expect_warning(
                expect_message(
                    expect_equal(x, descriptives_ref_2),
                    "[ sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85 ]",
                    fixed = TRUE
                ),
                "Potential disclosure problem: Dominant entities.",
                fixed = TRUE
            ),
            "Dominant entities in the breakdowns below:",
            fixed = TRUE
        )
    }

    # descriptives tests 2 ####
    descriptives_expect_2(
        sdc_descriptives(test_dt, id, val, by = sector)
    )
    descriptives_expect_2(
        sdc_descriptives(test_dt, id, val, by = .(sector))
    )
    descriptives_expect_2(
        sdc_descriptives(test_dt, id, val, by = list(sector))
    )
    descriptives_expect_2(
        sdc_descriptives(test_dt, id, val, by = "sector")
    )
    descriptives_expect_2(
        sdc_descriptives(test_dt, id, val, by = c("sector"))
    )
})

test_that("sdc_descriptices works in complex cases", {
    # descriptives setup 3 ####
    class(counts_ref_3)    <- c("sdc_counts"   , class(counts_ref_3))
    class(dominance_ref_3) <- c("sdc_dominance", class(dominance_ref_3))
    descriptives_ref_3 <- list(counts = counts_ref_3,
                               dominance = dominance_ref_3,
                               options = std_opts)

    descriptives_expect_3 <- function(x) {
        warnings <- capture_warnings({
            expect_output(
                expect_message(
                    expect_equal(x, descriptives_ref_3),
                    "[ sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85 ]",
                    fixed = TRUE
                ),
                "Dominant entities in the breakdowns below:",
                fixed = TRUE
            )
        })
        expect_match(
            warnings,
            "Potential disclosure problem\\: (Not enought distinct entities|Dominant entities)\\.")
    }

    # descriptives tests 3 ####
    descriptives_expect_3(
        sdc_descriptives(test_dt, id, val, by = .(sector, year))
    )
    descriptives_expect_3(
        sdc_descriptives(test_dt, id, val, by = list(sector, year))
    )
    descriptives_expect_3(
        sdc_descriptives(test_dt, id, val, by = "sector,year")
    )
    descriptives_expect_3(
        sdc_descriptives(test_dt, id, val, by = c("sector", "year"))
    )
    descriptives_expect_3(
        sdc_descriptives(test_dt, id, val, by = sector:year)
    )
})
