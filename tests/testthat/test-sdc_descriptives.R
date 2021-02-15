# test setup ####
library(data.table)
library(sdcLog)

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

## functionality tests
distinct_ids_ref_1 <- structure(
  data.table(distinct_ids = 10L),
  class = c("sdc_distinct_ids", "data.table", "data.frame")
)
distinct_ids_ref_2 <- structure(
  data.table(
    sector = c("S1", "S2"), distinct_ids = 5L, key = "sector"
  ),
  class = c("sdc_distinct_ids", "data.table", "data.frame")
)
distinct_ids_ref_3 <- structure(
  data.table(
    sector = c("S1", "S1", "S2", "S2"),
    year = rep(2019L:2020L, 2L),
    distinct_ids = c(4L, rep(5L, 3L)),
    key = c("sector", "year")
  ),
  class = c("sdc_distinct_ids", "data.table", "data.frame")
)

# test check_dominance ----

## functionality tests
dominance_ref_1 <- structure(
  data.table(value_share = 0.811146196943163),
  class = c("sdc_dominance", "data.table", "data.frame")
)
dominance_ref_2 <- structure(
  data.table(
    sector = c("S2", "S1"),
    value_share = c(0.888866740023071, 0.834414924858227)
  ),
  class = c("sdc_dominance", "data.table", "data.frame")
)
dominance_ref_3 <- structure(
  data.table(
    sector = c("S2", "S1", "S1", "S2"),
    year = c(rep(2020L, 2L), rep(2019L, 2L)),
    value_share = c(0.934568784234764, 0.913682312146633, 0.68150105511851, 0.550696457360742)
  ),
  class = c("sdc_dominance", "data.table", "data.frame")
)


# test sdc_descriptives ####
# descriptives setup 1 ####
descriptives_ref_1 <- structure(
  list(
    message_options = sdcLog:::message_options(),
    message_arguments = sdcLog:::message_arguments("id", "val", zero_as_NA = FALSE),
    distinct_ids = distinct_ids_ref_1,
    dominance = dominance_ref_1
  ),
  class = c("sdc_descriptives", "list")
)

expect_equal(
  sdc_descriptives(test_dt, "id", "val"),
  descriptives_ref_1
)

# descriptives setup 2 ####
descriptives_ref_2 <- structure(
  list(
    message_options = sdcLog:::message_options(),
    message_arguments = c(
      "[ SETTINGS: ",
      "id_var: id",
      " | val_var: val",
      " | by: sector",
      " | zero_as_NA: FALSE",
      " ]"
    ),
    distinct_ids = distinct_ids_ref_2,
    dominance = dominance_ref_2
  ),
  class = c("sdc_descriptives", "list")
)


# descriptives tests 2 ####
test_that("sdc_descriptives works in medium cases", {
  expect_warning(
    expect_equal(
      sdc_descriptives(test_dt, "id", "val", by = "sector"),
      descriptives_ref_2
    ),
    paste0(crayon::bold("DISCLOSURE PROBLEM: "), "Dominant entities."),
    fixed = TRUE
  )
})

# descriptives setup 3 ####
descriptives_ref_3 <- structure(
  list(
    message_options = sdcLog:::message_options(),
    message_arguments = c(
      "[ SETTINGS: ",
      "id_var: id",
      " | val_var: val",
      " | by: sector, year",
      " | zero_as_NA: FALSE",
      " ]"
    ),
    distinct_ids = distinct_ids_ref_3,
    dominance = dominance_ref_3
  ),
  class = c("sdc_descriptives", "list")
)

# descriptives tests 3 ####
test_that("sdc_descriptives works in complex cases", {
  warnings <- capture_warnings(
    expect_equal(
      sdc_descriptives(test_dt, "id", "val", by = c("sector", "year")),
      descriptives_ref_3
    )
  )
  expect_match(
    warnings,
    "DISCLOSURE PROBLEM:.*(Not enough distinct entities|Dominant entities)\\."
  )
})

options(sdc.info_level = 2)
descriptives_ref_4 <- structure(
  list(
    message_options = sdcLog:::message_options(),
    message_arguments = sdcLog:::message_arguments("id"),
    distinct_ids = distinct_ids_ref_1,
    dominance = structure(
      data.table(value_share = NA_real_),
      class = c("sdc_dominance", "data.table", "data.frame")
    )
  ),
  class = c("sdc_descriptives", "list")
)
test_that("sdc_descriptives() works without 'val_var'", {
  expect_identical(
    sdc_descriptives(test_dt, "id"),
    descriptives_ref_4
  )
})

# handling zeros ----
descriptives_ref_5 <- structure(
  list(
    message_options = sdcLog:::message_options(),
    message_arguments = sdcLog:::message_arguments(
      "id", "val", zero_as_NA = TRUE
    ),
    distinct_ids = distinct_ids_ref_1,
    dominance = dominance_ref_1
  ),
  class = c("sdc_descriptives", "list")
)

test_dt[is.na(val), val := 0]

test_that("zeros are handles correctly" , {
  expect_message(
    expect_equal(
      sdc_descriptives(test_dt, "id", "val"),
      descriptives_ref_5
    ),
    "A share of 0.05 of 'val_var' are zero. These will be treated as 'NA'.",
    fixed = TRUE
  )
  expect_silent(
    expect_equal(
      sdc_descriptives(test_dt, "id", "val", zero_as_NA = TRUE),
      descriptives_ref_5
    )
  )
})


# test that sdc_descriptives returns appropriate error
test_that("sdc_descriptives() returns appropriate error", {

  # throw error if data is not a data.frame
  expect_error(
    sdc_descriptives(wrong_test_dt, "id", "val_1"),
    "object 'wrong_test_dt' not found",
    fixed = TRUE
  )

  expect_error(
    sdc_descriptives("wrong_test_dt", "id", "val_1"),
    "Assertion on 'data' failed: Must be of type 'data.frame', not 'character'.",
    fixed = TRUE
  )

  # throw error if specified variables are not in data
  expect_error(
    sdc_descriptives(test_dt, "wrong_id", "val_1"),
    paste0(
      "Assertion on 'id_var' failed: Must be a subset of {'id','sector',",
      "'year','val'}, but is {'wrong_id'}."
    ),
    fixed = TRUE
  )
  expect_error(
    sdc_descriptives(test_dt, "id", "wrong_val"),
    paste0(
      "Assertion on 'val_var' failed: Must be a subset of {'sector',",
      "'year','val'}, but is {'wrong_val'}."
    ),
    fixed = TRUE
  )
  expect_error(
    sdc_descriptives(test_dt, "id", "val", "wrong_by"),
    paste0(
      "Assertion on 'by' failed: Must be a subset of {'sector','year'}, but is",
      " {'wrong_by'}."
    ),
    fixed = TRUE
  )

  # error for elements unquoted
  expect_error(
    sdc_descriptives(test_dt, id, "val_1"),
    "object 'id' not found"
  )
  expect_error(
    sdc_descriptives(test_dt, "id", val_1),
    "object 'val_1' not found"
  )

  # error for missing arguments
  expect_error(
    sdc_descriptives(id_var = "id", val_var = "val_1"),
    'argument "data" is missing, with no default',
    fixed = TRUE
  )
  expect_silent(
    sdc_descriptives(test_dt, "id")
  )

  options(sdc.id_var = NULL)
  expect_error(
    sdc_descriptives(test_dt, val_var = "val"),
    "Assertion on 'id_var' failed: Must be of type 'string', not 'NULL'.",
    fixed = TRUE
  )
})
