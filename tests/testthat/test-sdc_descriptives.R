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

# test check_distinct_ids ----
test_that("check_distinct_ids() returns a call", {
    expect_true(is.call(sdcLog:::check_distinct_ids(test_dt, "id", "val")))
    expect_type(sdcLog:::check_distinct_ids(test_dt, "id", "val"), "language")
})

## functionality tests
distinct_ids_ref_1 <- data.table(distinct_ids = integer())
distinct_ids_ref_2 <- data.table(sector = character(), distinct_ids = integer(),
                                 key = "sector")
distinct_ids_ref_3 <- data.table(sector = "S1", year = 2019L, distinct_ids = 4L,
                                 key = c("sector", "year"))
test_that("check_distinct_ids() distinct_ids correctly", {
    expect_identical(
        eval(sdcLog:::check_distinct_ids(test_dt, "id", "val")),
        distinct_ids_ref_1
    )
    expect_identical(
        eval(sdcLog:::check_distinct_ids(test_dt, "id", "val", by = sector)),
        distinct_ids_ref_2
    )
    expect_identical(
        eval(sdcLog:::check_distinct_ids(test_dt, "id", "val",
                                         by = .(sector, year))),
        distinct_ids_ref_3
    )
})

# test check_dominance ----
## pure technical tests
test_that("check_dominance() returns a call", {
    expect_true(is.call(sdcLog:::check_dominance(test_dt, "id", "val")))
    expect_type(sdcLog:::check_dominance(test_dt, "id", "val"), "language")
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

test_that("check_dominance() calculates correctly", {
    expect_identical(
        eval(sdcLog:::check_dominance(test_dt, "id", "val")),
        dominance_ref_1
    )
    expect_equal(
        eval(sdcLog:::check_dominance(test_dt, "id", "val", by = sector)),
        dominance_ref_2
    )
    expect_equal(
        eval(sdcLog:::check_dominance(test_dt, "id", "val",
                                      by = .(sector, year))),
        dominance_ref_3
    )
})


# test sdc_descriptives ####
# descriptives setup 1 ####
class(distinct_ids_ref_1) <- c("sdc_distinct_ids", class(distinct_ids_ref_1))
class(dominance_ref_1) <- c("sdc_dominance", class(dominance_ref_1))
descriptives_ref_1 <- list(
    message_options = message_options(),
    message_arguments = message_arguments("id", "val", zero_as_NA = FALSE),
    distinct_ids = distinct_ids_ref_1,
    dominance = dominance_ref_1
)
class(descriptives_ref_1) <- c("sdc_descriptives", class(descriptives_ref_1))

# descriptives_expect_1 <- function(x) {
#     messages <- capture_messages(expect_identical(x, descriptives_ref_1))
#     expect_match(
#         paste0(messages, collapse = ""),
#         paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | ",
#                "sdc.share_dominance: 0.85 ]\n",
#                "[ SETTINGS: id_var: id | val_var: val ]\n",
#                collapse = ""),
#         fixed = TRUE
#     )
# }

# descriptives tests 1 ####
test_that("sdc_descriptives works in simple cases", {
    expect_equal(
        sdc_descriptives(test_dt, "id", "val"), descriptives_ref_1,
        ignore_attr = TRUE
    )
})

# descriptives setup 2 ####
class(distinct_ids_ref_2) <- c("sdc_distinct_ids", class(distinct_ids_ref_2))
class(dominance_ref_2) <- c("sdc_dominance", class(dominance_ref_2))


descriptives_ref_2 <- list(
    message_options = message_options(),
    message_arguments = c(
        "[ SETTINGS: ",
        paste0("id_var: ", "id"),
        paste0(" | val_var: ", "val"),
        paste0(" | by: ", "sector"),
        paste0(" | zero_as_NA: FALSE"),
        " ]"
    ),
    distinct_ids = distinct_ids_ref_2,
    dominance = dominance_ref_2)
class(descriptives_ref_2) <- c("sdc_descriptives", class(descriptives_ref_2))

descriptives_expect_2 <- function(x) {
    expect_warning(
        expect_equal(x, descriptives_ref_2, ignore_attr = TRUE),
        ifelse(getOption("sdc.info_level", 1L) > 1L,
               paste0(bold("Potential disclosure problem: "),"Dominant entities."), ""),
        fixed = TRUE
    )
}

# descriptives tests 2 ####
test_that("sdc_descriptives works in medium cases", {
    descriptives_expect_2(
        sdc_descriptives(test_dt, "id", "val", by = sector)
    )
    descriptives_expect_2(
        sdc_descriptives(test_dt, "id", "val", by = .(sector))
    )
    descriptives_expect_2(
        sdc_descriptives(test_dt, "id", "val", by = list(sector))
    )
    descriptives_expect_2(
        sdc_descriptives(test_dt, "id", "val", by = "sector")
    )
    descriptives_expect_2(
        sdc_descriptives(test_dt, "id", "val", by = c("sector"))
    )
})


# descriptives setup 3 ####
class(distinct_ids_ref_3)    <- c("sdc_distinct_ids"   , class(distinct_ids_ref_3))
class(dominance_ref_3) <- c("sdc_dominance", class(dominance_ref_3))
descriptives_ref_3 <- list(
    message_options = message_options(),
    message_arguments = c(
        "[ SETTINGS: ",
        paste0("id_var: ", "id"),
        paste0(" | val_var: ", "val"),
        paste0(" | by: ", "sector, year"),
        paste0(" | zero_as_NA: FALSE"),
        " ]"
    ),
    distinct_ids = distinct_ids_ref_3,
    dominance = dominance_ref_3
)
class(descriptives_ref_3) <- c("sdc_descriptives", class(descriptives_ref_3))

## expect identical??
descriptives_expect_3 <- function(x) {
    warnings <- capture_warnings(expect_equal(
        sdc_descriptives(test_dt, "id", "val", by = .(sector, year)),
        descriptives_ref_3,
        ignore_attr = TRUE
    ))
    warnings <- gsub("\\\033\\[[1-2]{1,2}m", "", warnings)
    expect_match(
        warnings[1],
        "Potential disclosure problem: Not enough distinct entities.",
        fixed = TRUE
    )
    expect_match(
        warnings[2],
        "Potential disclosure problem: Dominant entities.",
        fixed = TRUE
    )
}

# descriptives tests 3 ####
test_that("sdc_descriptives works in complex cases", {
    descriptives_expect_3(
        sdc_descriptives(test_dt, "id", "val", by = .(sector, year))
    )
    descriptives_expect_3(
        sdc_descriptives(test_dt, "id", "val", by = list(sector, year))
    )
    descriptives_expect_3(
        sdc_descriptives(test_dt, "id", "val", by = "sector,year")
    )
    descriptives_expect_3(
        sdc_descriptives(test_dt, "id", "val", by = c("sector", "year"))
    )
    descriptives_expect_3(
        sdc_descriptives(test_dt, "id", "val", by = sector:year)
    )
})



# test arguments in sdc_descriptives

# test that sdc_descriptives retruns appropriate error
test_that("sdc_descriptives() returns appropriate error", {

    # error für nichtexistierende Elemente
    expect_error(sdc_descriptives(test_dt, "wrong_id", "val"), "Some items of .SDcols are not column names: [wrong_id]", fixed = TRUE)
    expect_error(sdc_descriptives(test_dt, "id", "wrong_val"), "Object 'wrong_val' not found amongst id, sector, year, val")
    expect_error(sdc_descriptives(wrong_test_dt, "id", "val"), "object 'wrong_test_dt' not found")
    expect_error(sdc_descriptives(test_dt, "id", "val", wrong_by), "object 'wrong_by' not found")

    # error für elements unquoted
    expect_error(sdc_descriptives(test_dt, id, "val"), "object 'id' not found")
    expect_error(sdc_descriptives(test_dt, "id", val), "object 'val' not found")

    # error für data quoted
    expect_error(sdc_descriptives("test_dt", "id", "val"), "Assertion on 'data' failed: Must be of type 'data.frame', not 'character'.")

    # error für missing arguments
    expect_error(sdc_descriptives(test_dt, "id"), "argument \"val_var\" is missing, with no default")
    expect_error(sdc_descriptives(test_dt, val_var = "val"), "argument \"id_var\" is missing, with no default")
    expect_error(sdc_descriptives(id = "id", val_var = "val"), "argument \"data\" is missing, with no default")

})

# Tests für NA_vals
n <- 20L
NA_vals_test_dt <- data.table(
    id = rep_len(LETTERS[1L:10L], n),
    year = sort(rep_len(2019L:2020L, n)),
    val = runif(n, min = 1, max = 10),
    val_3 = c(rep(0, 16), runif(4, min = 1, max = 10)),
    key = "id"
)


# test that sdc_descriptives handles NA values correctly
test_that("sdc_descriptives() handles NA values correctly", {

    expect_warning(
        sdc_descriptives(NA_vals_test_dt, "id", "val_3", zero_as_NA = TRUE)
    )

    expect_message(
        expect_warning(
            sdc_descriptives(NA_vals_test_dt, "id", "val_3")
        ),
        paste0(
            "A share of 0.8 of 'val_var' are zero. These will be treated as 'NA'.\n",
            "To prevent this behaviour and / or avoid this message, set ",
            "'zero_as_NA' explicitly."),
        fixed = TRUE
    )


    expect_message(
        expect_warning(
            sdc_descriptives(NA_vals_test_dt, "id", "val_3")
        ),
        paste0(
            "A share of 0.8 of 'val_var' are zero. These will be treated as 'NA'.\n",
            "To prevent this behaviour and / or avoid this message, set ",
            "'zero_as_NA' explicitly."),
        fixed = TRUE
    )
})
