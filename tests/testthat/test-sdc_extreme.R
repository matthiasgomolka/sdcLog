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


# test sdc_extreme ----
# simple case ----
# calculate extreme values for val_var = val_1
extreme_ref_1 <- data.table(
  val_var = "val_1",
  min = extreme_test_dt[6L:10L, mean(val_1)],
  n_obs_min = 5L,
  max = extreme_test_dt[1L:5L, mean(val_1)],
  n_obs_max = 5L
)

# create test function
extreme_expect_1 <- function(x) {
  messages <- capture_messages(expect_identical(x, extreme_ref_1))
  expect_match(
    paste0(messages, collapse = ""),
    paste0(
      "[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | ",
      "sdc.share_dominance: 0.85 ]\n",
      "[ SETTINGS: id_var: id | val_var: val_1 ]\n",
      collapse = ""
    ),
    fixed = TRUE
  )
}

# actual test
test_that("sdc_extreme() works in simple cases", {
  extreme_expect_1(
    sdc_extreme(extreme_test_dt, "id", "val_1")
  )
})


# problematic case 1 ----
# problem with dominance for 1:5, so 1:9 necessary but would lead to overlap, so
# all extreme values NA
extreme_ref_2 <- data.table(
  val_var = "val_2",
  min = NA_real_,
  n_obs_min = NA_integer_,
  max = NA_real_,
  n_obs_max = NA_integer_
)

# create test function
extreme_expect_2 <- function(x) {
  messages <- capture_messages(expect_identical(x, extreme_ref_2))
  expect_match(
    paste0(messages, collapse = ""),
    paste0(
      "[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | ",
      "sdc.share_dominance: 0.85 ]\n",
      "[ SETTINGS: id_var: id | val_var: val_2 ]\n",
      "It is impossible to compute extreme values for variable 'val_2' that ",
      "comply to RDC rules.",
      collapse = ""
    ),
    fixed = TRUE
  )
}

# actual test
test_that("sdc_extreme() produces no result in case of sd_overlap", {
  extreme_expect_2(
    sdc_extreme(extreme_test_dt, "id", "val_2")
  )
})

# problematic case 2 ----
# problem with NA in (1:5), so 2:6 necessary but would lead to overlap, so all
# extreme values NA
extreme_ref_3 <- copy(extreme_ref_2)
extreme_ref_3[, val_var := "val_3"]

# create test function
extreme_expect_3 <- function(x) {
  messages <- capture_messages(expect_identical(x, extreme_ref_3))
  expect_match(
    paste0(messages, collapse = ""),
    paste0(
      "[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | ",
      "sdc.share_dominance: 0.85 ]\n",
      "[ SETTINGS: id_var: id | val_var: val_3 ]\n",
      "It is impossible to compute extreme values for variable 'val_3' that ",
      "comply to RDC rules.",
      collapse = ""
    ),
    fixed = TRUE
  )
}

# actual test
test_that("sdc_extreme() produces no result in case of sd_overlap", {
  extreme_expect_3(
    sdc_extreme(extreme_test_dt, "id", "val_3")
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
extreme_ref_4 <- data.table(
  val_var = "val_1",
  sector = c("S1", "S2"),
  min = c(
    extreme_test_dt_by[6L:10L, mean(val_1)],
    extreme_test_dt_by[16L:20L, mean(val_1)]
  ),
  n_obs_min = 5L,
  max = c(
    extreme_test_dt_by[1L:5L, mean(val_1)],
    extreme_test_dt_by[11L:15L, mean(val_1)]
  ),
  n_obs_max = 5L,
  key = "sector"
)

# create test function
extreme_expect_4 <- function(x) {
  messages <- capture_messages(expect_identical(x, extreme_ref_4))
  expect_match(
    paste0(messages, collapse = ""),
    paste0(
      "[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | ",
      "sdc.share_dominance: 0.85 ]\n",
      "[ SETTINGS: id_var: id | val_var: val_1 | by: sector ]\n",
      collapse = ""
    ),
    fixed = TRUE
  )
}

# actual tests
test_that("sdc_extreme() works in simple cases with by", {
  extreme_expect_4(
    sdc_extreme(extreme_test_dt_by, "id", "val_1", by = sector)
  )
  extreme_expect_4(
    sdc_extreme(extreme_test_dt_by, "id", "val_1", by = "sector")
  )
  extreme_expect_4(
    sdc_extreme(extreme_test_dt_by, "id", "val_1", by = .(sector))
  )
})


# problematic by case 1 ----
# setup test extreme values for val_var = val_2, by = sector
extreme_ref_5 <- copy(extreme_ref_4)
extreme_ref_5[, val_var := "val_2"]
for (var in c("min", "max")) {
  set(extreme_ref_5, j = var, value = NA_real_)
}
for (var in c("n_obs_min", "n_obs_max")) {
  set(extreme_ref_5, j = var, value = NA_integer_)
}

# create test function
extreme_expect_5 <- function(x) {
  messages <- capture_messages(expect_identical(x, extreme_ref_5))
  expect_match(
    paste0(messages, collapse = ""),
    paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | ",
      "sdc.share_dominance: 0.85 ]\n",
      "[ SETTINGS: id_var: id | val_var: val_2 | by: sector ]\n",
      "It is impossible to compute extreme values for variable 'val_2' ",
      "that comply to RDC rules.",
      collapse = ""
    ),
    fixed = TRUE
  )
}

# actual tests
test_that("sdc_extreme() gives no result in by cases", {
  extreme_expect_5(
    sdc_extreme(extreme_test_dt_by, "id", "val_2", by = sector)
  )
  extreme_expect_5(
    sdc_extreme(extreme_test_dt_by, "id", "val_2", by = "sector")
  )
  extreme_expect_5(
    sdc_extreme(extreme_test_dt_by, "id", "val_2", by = .(sector))
  )
})


# tests for internal functions ----

# find_SD_problems:
# test that find_SD_problems returns a list
test_that("find_SD_problems() returns a list", {
  expect_type(
    find_SD_problems(
      extreme_test_dt, head, 5L, "id", "val_1",
      by = NULL
    ),
    "list"
  )
  expect_type(find_SD_problems(
    extreme_test_dt, tail, 5L, "id", "val_1",
    by = NULL
  ), "list")
})

# test that find_SD_problems detects problems correctly

test_that("find_SD_problems() detects problems correctly", {
  # case 1: extreme_test_dt:
  # no problems with subset of head/tail 5 obs. for val_var = val_1 so
  # list[["problems]] == FALSE
  expect_false(find_SD_problems(
    extreme_test_dt, head, 5L, "id", "val_1",
    by = NULL
  )[["problems"]])
  expect_false(find_SD_problems(
    extreme_test_dt, tail, 5L, "id", "val_1",
    by = NULL
  )[["problems"]])

  # case 2: extreme_test_dt: problems with subset of head 5 obs. for
  # val_var = val_2, no problems for tail so list[["problems"]] == TRUE for
  # head, for tail == FALSE
  expect_true(find_SD_problems(
    extreme_test_dt, head, 5L, "id", "val_2",
    by = NULL
  )[["problems"]])
  expect_false(find_SD_problems(
    extreme_test_dt, tail, 5L, "id", "val_2",
    by = NULL
  )[["problems"]])

  # case 3: extreme_test_dt: no problems with subset of head/tail 5 obs.
  # for val_var = val so list[["problems]] == FALSE but NA's have to be excluded
  # the same way as in function sdc_extreme
  case_3 <- na.omit(extreme_test_dt, cols = "val_3")
  expect_false(find_SD_problems(
    case_3, head, 5L, "id", "val_3",
    by = NULL
  )[["problems"]])
  expect_false(find_SD_problems(
    case_3, tail, 5L, "id", "val_3",
    by = NULL
  )[["problems"]])

  # case 4: extreme_test_dt_by: no problems with subset of head/tail 5 obs.
  # for val_var = val and by = "sector" so list[["problems]] == FALSE
  expect_false(find_SD_problems(
    extreme_test_dt_by, head, 5L, "id", "val_1", "sector"
  )[["problems"]])
  expect_false(find_SD_problems(
    extreme_test_dt_by, tail, 5L, "id", "val_1", "sector"
  )[["problems"]])

  # case 5: extreme_test_dt_by: problems with subset of head 5 obs.
  # for val_var = val_2 and by = "sector", no problems for tail so
  # list[["problems"]] == TRUE for head, for tail == FALSE
  expect_true(find_SD_problems(
    extreme_test_dt_by, head, 5L, "id", "val_2", "sector"
  )[["problems"]])
  expect_false(find_SD_problems(
    extreme_test_dt_by, tail, 5L, "id", "val_2", "sector"
  )[["problems"]])

  # case 6: extreme_test_dt_by: problems with subset of head & tail 5 obs.
  # for val_var = val_4 and by = "sector" so list[["problems"]] == TRUE but NA's
  # have to be excluded the same way as in function sdc_extreme:
  case_6 <- na.omit(extreme_test_dt_by, cols = "val_4")
  expect_true(find_SD_problems(
    case_6, head, 5L, "id", "val_4", "sector"
  )[["problems"]])
  expect_true(find_SD_problems(
    case_6, tail, 5L, "id", "val_4", "sector"
  )[["problems"]])
})


# find SD problems ----
# test that find_SD returns correct subset
row_order <- c("sector", "id", "val_1", "val_2", "val_3", "val_4")

test_that("find_SD() returns correct subset", {
  # setup 1: subset for val_var = val
  # no problems at all, so subset max: 1:5, subset min: 6:10
  expect_identical(
    find_SD(extreme_test_dt, "min", 5L, "id", "val_1", by = NULL),
    extreme_test_dt[6L:10L, ]
  )
  expect_identical(
    find_SD(extreme_test_dt, "max", 5L, "id", "val_1", by = NULL),
    extreme_test_dt[1L:5L, ]
  )

  # setup 2: subset for val_var = val_2
  # problems with dominance for max, so subset max: 1:9, subset min: 6:10
  expect_identical(
    find_SD(extreme_test_dt, "min", 5L, "id", "val_1", by = NULL),
    extreme_test_dt[6L:10L, ]
  )
  expect_identical(
    find_SD(extreme_test_dt, "max", 9L, "id", "val_1", by = NULL),
    extreme_test_dt[1L:9L, ]
  )

  # setup 3: subset for val_var = "val" and by = "sector"
  # no problems at all, so subsets with different col order
  expect_identical(
    find_SD(extreme_test_dt_by, "min", 5L, "id", "val_1", "sector"),
    extreme_test_dt_by[c(6L:10L, 16L:20L), row_order, with = FALSE]
  )
  expect_identical(
    find_SD(extreme_test_dt_by, "max", 5L, "id", "val_1", "sector"),
    extreme_test_dt_by[c(1L:5L, 11L:15L), row_order, with = FALSE]
  )
})



# test arguments in sdc_extreme ----

# test that sdc_extreme retruns appropriate error
test_that("sdc_extreme() returns appropriate error", {

  # error für nichtexistierende Elemente
  suppressMessages({
    # messages sind hier nicht wichtig, deshalb suppressed
    expect_error(sdc_extreme(extreme_test_dt, "wrong_id", "val_1"), "Some items of .SDcols are not column names: [wrong_id]", fixed = TRUE)
    expect_error(sdc_extreme(extreme_test_dt, "id", "wrong_val"), "argument specifying columns specify non existing column(s): cols[1]='wrong_val'", fixed = TRUE)
    expect_error(sdc_extreme(wrong_test_dt, "id", "val_1"), "object 'wrong_test_dt' not found")
    expect_error(sdc_extreme(extreme_test_dt_by, "id", "val_1", wrong_by), "object 'wrong_by' not found")

    # error für elements unquoted
    expect_error(sdc_extreme(extreme_test_dt, id, "val_1"), "object 'id' not found")
    expect_error(sdc_extreme(extreme_test_dt, "id", val_1), "object 'val_1' not found")

    # error für data quoted
    expect_error(sdc_extreme("extreme_test_dt", "id", "val_1"), "Assertion on 'data' failed: Must be of type 'data.frame', not 'character'.")

    # error für missing arguments
    expect_error(sdc_extreme(extreme_test_dt, "id"), "argument \"val_var\" is missing, with no default")
    expect_error(sdc_extreme(extreme_test_dt, val_var = "val_1"), "argument \"id_var\" is missing, with no default")
    expect_error(sdc_extreme(id = "id", val_var = "val_1"), "argument \"data\" is missing, with no default")
  })
})

# no infinite loop ----
test_that("sdc_extreme() does not enter an infinite loop", {
  messages <- capture_messages(sdc_extreme(extreme_test_dt[1:5], "id", "val_2"))
  expect_match(
    paste0(messages, collapse = ""),
    paste0(
      "[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | ",
      "sdc.share_dominance: 0.85 ]\n",
      "[ SETTINGS: id_var: id | val_var: val_2 ]\n",
      "It is impossible to compute extreme values for variable 'val_2' that ",
      "comply to RDC rules.",
      collapse = ""
    ),
    fixed = TRUE
  )
})

