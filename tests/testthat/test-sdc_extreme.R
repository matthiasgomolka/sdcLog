library(data.table)
set.seed(1L)

# small test dt
extreme_test_dt <- data.table(
  id = LETTERS[1L:10L],
  val_1 = 10L:1L,
  val_2 = c(100L, 90L, 8L:1L),
  val_3 = c(NA_integer_, 9L:1L),
  key = "id"
)


# test sdc_min_max ----
# simple case ----
# calculate extreme values for val_var = val_1
extreme_ref_1 <- structure(
  list(
    message_options = sdcLog:::message_options(),
    message_arguments = sdcLog:::message_arguments("id", "val_1"),
    min_max = data.table(
      val_var = "val_1",
      min = extreme_test_dt[6L:10L, mean(val_1)],
      distinct_ids_min = 5L,
      max = extreme_test_dt[1L:5L, mean(val_1)],
      distinct_ids_max = 5L
    )
  ),
  class = c("sdc_min_max", "list")
)

test_that("sdc_min_max() works in simple case", {
  expect_identical(
    sdc_min_max(extreme_test_dt, "id", "val_1"),
    extreme_ref_1
  )
})

# problematic case 1 ----
# problem with dominance for 1:5, so 1:9 necessary but would lead to overlap, so
# all extreme values NA
extreme_ref_2 <- structure(
  list(
    message_options = sdcLog:::message_options(),
    message_arguments = sdcLog:::message_arguments("id", "val_2"),
    min_max = data.table(
      val_var = "val_2",
      min = NA_real_,
      distinct_ids_min = NA_integer_,
      max = NA_real_,
      distinct_ids_max = NA_integer_
    )
  ),
  class = c("sdc_min_max", "list")
)

test_that("sdc_min_max() produces no result in case of sd_overlap due to dominance", {
  expect_message(
    expect_equal(
      sdc_min_max(extreme_test_dt, "id", "val_2"),
      extreme_ref_2
    ),
    "It is impossible to compute extreme values for variable 'val_2' that comply to RDC rules.",
    fixed = TRUE
  )
})

# problematic case 2 ----
# problem with NA in (1:5), so 2:6 necessary but would lead to overlap, so all
# extreme values NA
extreme_ref_3 <- structure(
  list(
    message_options = sdcLog:::message_options(),
    message_arguments = sdcLog:::message_arguments("id", "val_3"),
    min_max = data.table(
      val_var = "val_3",
      min = NA_real_,
      distinct_ids_min = NA_integer_,
      max = NA_real_,
      distinct_ids_max = NA_integer_
    )
  ),
  class = c("sdc_min_max", "list")
)

# actual test
test_that("sdc_min_max() produces no result in case of sd_overlap", {
  expect_message(
    expect_identical(
      sdc_min_max(extreme_test_dt, "id", "val_3"),
      extreme_ref_3
    ),
    "It is impossible to compute extreme values for variable 'val_3' that comply to RDC rules.",
    fixed = TRUE
  )
})


# create test dt für by argument ----
n <- 20L
extreme_test_dt_by <- data.table(
  id = rep_len(LETTERS[1L:10L], n),
  sector = sort(rep_len(paste0("S", 1L:2L), n)),
  val_1 = 20L:1L,
  val_2 = c(200L, 190L, 18L:1L),
  val_3 = c(NA_integer_, 19L:1L),
  val_4 = c(20L:7L, rep(NA_integer_, 6L)),
  key = "id"
)
setorder(extreme_test_dt_by, -val_1)

# simple by case ----
# setup test extreme values for val_var = val, by = sector
extreme_ref_4 <- structure(
  list(
    message_options = sdcLog:::message_options(),
    message_arguments = sdcLog:::message_arguments("id", "val_1", "sector"),
    min_max = data.table(
      val_var = "val_1",
      sector = c("S1", "S2"),
      min = c(
        extreme_test_dt_by[6L:10L, mean(val_1)],
        extreme_test_dt_by[16L:20L, mean(val_1)]
      ),
      distinct_ids_min = 5L,
      max = c(
        extreme_test_dt_by[1L:5L, mean(val_1)],
        extreme_test_dt_by[11L:15L, mean(val_1)]
      ),
      distinct_ids_max = 5L,
      key = "sector"
    )
  ),
  class = c("sdc_min_max", "list")
)

# actual tests
test_that("sdc_min_max() works in simple cases with by", {
  # expect_identical(
  #   sdc_min_max(extreme_test_dt_by, "id", "val_1", by = sector),
  #   extreme_ref_4
  # )
  expect_identical(
    sdc_min_max(extreme_test_dt_by, "id", "val_1", by = "sector"),
    extreme_ref_4
  )
  # expect_identical(
  #   sdc_min_max(extreme_test_dt_by, "id", "val_1", by = .(sector)),
  #   extreme_ref_4
  # )
})


# problematic by case 1 ----
# setup test extreme values for val_var = val_2, by = sector
extreme_ref_5 <- structure(
  list(
    message_options = sdcLog:::message_options(),
    message_arguments = sdcLog:::message_arguments("id", "val_2", "sector"),
    min_max = data.table(
      val_var = "val_2",
      sector = c("S1", "S2"),
      min = NA_real_,
      distinct_ids_min = NA_integer_,
      max = NA_real_,
      distinct_ids_max = NA_integer_,
      key = "sector"
    )
  ),
  class = c("sdc_min_max", "list")
)

# actual tests
test_that("sdc_min_max() gives no result in by cases", {
  expect_message(
    expect_identical(
      sdc_min_max(extreme_test_dt_by, "id", "val_2", by = "sector"),
      extreme_ref_5
    ),
    "It is impossible to compute extreme values for variable 'val_2' that comply to RDC rules.",
    fixed = TRUE
  )
})




# tests for internal functions (removed, since not necessary) ----


# test arguments in sdc_min_max ----

# test that sdc_min_max returns appropriate error
test_that("sdc_min_max() returns appropriate error", {

  # throw error if data is not a data.frame
  expect_error(
    sdc_min_max(wrong_test_dt, "id", "val_1"),
    "object 'wrong_test_dt' not found",
    fixed = TRUE
  )

  expect_error(
    sdc_min_max("wrong_test_dt", "id", "val_1"),
    "Assertion on 'data' failed: Must be of type 'data.frame', not 'character'.",
    fixed = TRUE
  )

  # throw error if specified variables are not in data
  expect_error(
    sdc_min_max(extreme_test_dt, "wrong_id", "val_1"),
    paste0(
      "Assertion on 'id_var' failed: Must be a subset of {'id','val_1',",
      "'val_2','val_3'}, but is {'wrong_id'}."
    ),
    fixed = TRUE
  )
  expect_error(
    sdc_min_max(extreme_test_dt, "id", "wrong_val"),
    paste0(
      "Assertion on 'val_var' failed: Must be a subset of {'val_1',",
      "'val_2','val_3'}, but is {'wrong_val'}."
    ),
    fixed = TRUE
  )
  expect_error(
    sdc_min_max(extreme_test_dt_by, "id", "val_1", "wrong_by"),
    paste0(
      "Assertion on 'by' failed: Must be a subset of {'sector',",
      "'val_2','val_3','val_4'}, but is {'wrong_by'}."
    ),
    fixed = TRUE
  )

  # error for elements unquoted
  expect_error(
    sdc_min_max(extreme_test_dt, id, "val_1"),
    "object 'id' not found"
  )
  expect_error(
    sdc_min_max(extreme_test_dt, "id", val_1),
    "object 'val_1' not found"
  )

  # error for missing arguments
  expect_error(
    sdc_min_max(id_var = "id", val_var = "val_1"),
    'argument "data" is missing, with no default',
    fixed = TRUE
  )
  expect_error(
    sdc_min_max(extreme_test_dt, "id"),
    'argument "val_var" is missing, with no default',
    fixed = TRUE
  )
  expect_error(
    sdc_min_max(extreme_test_dt, val_var = "val_1"),
    "Assertion on 'id_var' failed: Must be of type 'string', not 'NULL'.",
    fixed = TRUE
  )
  expect_error(
    sdc_min_max(extreme_test_dt, "id_var", val_var = "val_1"),
    "Assertion on 'id_var' failed: Must be a subset of {'id','val_1','val_2','val_3'}, but is {'id_var'}.",
    fixed = TRUE
  )
  expect_error(
    sdc_min_max(extreme_test_dt, "id", val_var = "val_"),
    "Assertion on 'val_var' failed: Must be a subset of {'val_1','val_2','val_3'}, but is {'val_'}.",
    fixed = TRUE
  )
  expect_error(
    sdc_min_max(extreme_test_dt, "id", val_var = "val_1", by = "by_var"),
    "Assertion on 'by' failed: Must be a subset of {'val_2','val_3'}, but is {'by_var'}.",
    fixed = TRUE
  )
})

# no infinite loop ----

test_that("sdc_min_max() does not enter an infinite loop", {
  expect_message(
    expect_identical(
      sdc_min_max(extreme_test_dt[1:5], "id", "val_2"),
      extreme_ref_2
    ),
    "It is impossible to compute extreme values for variable 'val_2' that comply to RDC rules.",
    fixed = TRUE
  )
})

