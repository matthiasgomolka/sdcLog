# test setup ####
library(data.table)

# simple cases ----
distinct_ids_ref_1 <- structure(
  data.table(distinct_ids = 10L),
  class = c("sdc_distinct_ids", "data.table", "data.frame")
)
dominance_ref_1 <- structure(
  data.table(value_share = 0.7413301),
  class = c("sdc_dominance", "data.table", "data.frame")
)
descriptives_ref_1 <- structure(
  list(
    options = sdcLog:::list_options(),
    settings = sdcLog:::list_arguments("id", "val_1", zero_as_NA = FALSE),
    distinct_ids = distinct_ids_ref_1,
    dominance = dominance_ref_1
  ),
  class = c("sdc_descriptives", "list")
)

test_that("sdc_descriptives works in simple cases", {
  data("sdc_descriptives_DT")

  expect_equal(
    sdc_descriptives(sdc_descriptives_DT, "id", "val_1"),
    descriptives_ref_1,
    ignore_attr = TRUE
  )

  expect_equal(
    sdc_descriptives(as.data.frame(sdc_descriptives_DT), "id", "val_1"),
    descriptives_ref_1,
    ignore_attr = TRUE
  )

  if (requireNamespace("tibble", quietly = TRUE)) {
    expect_equal(
      sdc_descriptives(tibble::as_tibble(sdc_descriptives_DT), "id", "val_1"),
      descriptives_ref_1,
      ignore_attr = TRUE
    )
  }
})


# medium cases ----
test_that("sdc_descriptives works in medium cases", {
  data("sdc_descriptives_DT")

  distinct_ids_ref_2 <- structure(
    data.table(
      sector = factor(c("S1", "S2")),
      distinct_ids = 4L:5L,
      key = "sector"
    ),
    class = c("sdc_distinct_ids", "data.table", "data.frame")
  )
  dominance_ref_2 <- structure(
    data.table(
      sector = factor(c("S2", "S1")),
      value_share = c(0.84650585, 0.60400376)
    ),
    class = c("sdc_dominance", "data.table", "data.frame")
  )
  descriptives_ref_2 <- structure(
    list(
      options = list_options(),
      settings = list_arguments(id_var = "id", val_var = "val_1", by = "sector", zero_as_NA = FALSE),
      distinct_ids = distinct_ids_ref_2,
      dominance = dominance_ref_2
    ),
    class = c("sdc_descriptives", "list")
  )
  expect_warning(
    expect_equal(
      sdc_descriptives(sdc_descriptives_DT[!(id == "A")], "id", "val_1", by = "sector"),
      descriptives_ref_2,
      ignore_attr = TRUE
    ),
    paste(
        cli::style_bold("DISCLOSURE PROBLEM:"), "Not enough distinct entities."
    ),
    fixed = TRUE
  )
})


# complex cases ----
test_that("sdc_descriptives works in complex cases", {
  data("sdc_descriptives_DT")

  distinct_ids_ref_3 <- structure(
    data.table(
      sector = factor(c("S1", "S1", "S2", "S2")),
      year = rep(2019L:2020L, 2L),
      distinct_ids = c(4L, rep(5L, 3L)),
      key = c("sector", "year")
    ),
    class = c("sdc_distinct_ids", "data.table", "data.frame")
  )
  dominance_ref_3 <- structure(
    data.table(
      sector = factor(c("S2", "S1", "S1", "S2")),
      year = c(rep(2020L, 2L), rep(2019L, 2L)),
      value_share = c(0.90563139, 0.87768517, 0.6815010551185098, 0.5506964573607419)
    ),
    class = c("sdc_dominance", "data.table", "data.frame")
  )

  descriptives_ref_3 <- structure(
    list(
      options = sdcLog:::list_options(),
      settings = list_arguments(id_var = "id", val_var = "val_1", by = c("sector", "year"), zero_as_NA = FALSE),
      distinct_ids = distinct_ids_ref_3,
      dominance = dominance_ref_3
    ),
    class = c("sdc_descriptives", "list")
  )

  warnings <- capture_warnings(
    expect_equal(
      sdc_descriptives(sdc_descriptives_DT, "id", "val_1", by = c("sector", "year")),
      descriptives_ref_3,
      ignore_attr = TRUE
    )
  )
  expect_match(
    warnings,
    "DISCLOSURE PROBLEM:.*(Not enough distinct entities|Dominant entities)\\."
  )
})


# no val_var ----
test_that("sdc_descriptives() works without 'val_var'", {
  data("sdc_descriptives_DT")
  options(sdc.info_level = 2)

  descriptives_ref_4 <- structure(
    list(
      options = sdcLog:::list_options(),
      settings = sdcLog:::list_arguments("id"),
      distinct_ids = distinct_ids_ref_1,
      dominance = structure(
        data.table(value_share = NA_real_),
        class = c("sdc_dominance", "data.table", "data.frame")
      )
    ),
    class = c("sdc_descriptives", "list")
  )
  expect_equal(
    sdc_descriptives(sdc_descriptives_DT, "id"),
    descriptives_ref_4,
    ignore_attr = TRUE
  )
})


# handling zeros ----
test_that("zeros are handles correctly", {
  data("sdc_descriptives_DT")
  sdc_descriptives_DT_copy <- sdc_descriptives_DT

  descriptives_ref_5 <- structure(
    list(
      options = sdcLog:::list_options(),
      settings = sdcLog:::list_arguments(
        "id", "val_2", zero_as_NA = TRUE
      ),
      distinct_ids = distinct_ids_ref_1,
      dominance = structure(
        data.table(value_share = 0.35958732),
        class = c("sdc_dominance", "data.table", "data.frame")
      )
    ),
    class = c("sdc_descriptives", "list")
  )

  expect_message(
    expect_equal(
      sdc_descriptives(sdc_descriptives_DT, "id", "val_2"),
      descriptives_ref_5,
      ignore_attr = TRUE
    ),
    "A share of 0.2 of 'val_var' are zero. These will be treated as 'NA'.",
    fixed = TRUE
  )

  expect_silent(
    expect_equal(
      sdc_descriptives(sdc_descriptives_DT, "id", "val_2", zero_as_NA = TRUE),
      descriptives_ref_5,
      ignore_attr = TRUE
    ))

  # assert that input data remains unchanged
  expect_identical(sdc_descriptives_DT, sdc_descriptives_DT_copy)
})


# errors ----
test_that("sdc_descriptives() returns appropriate error", {
  data("sdc_descriptives_DT")

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
    sdc_descriptives(sdc_descriptives_DT, "wrong_id", "val_1"),
    "'id_var'.*subset"
  )
  expect_error(
    sdc_descriptives(sdc_descriptives_DT, "id", "wrong_val"),
    "'val_var'.*subset"
  )
  expect_error(
    sdc_descriptives(sdc_descriptives_DT, "id", "val_1", "wrong_by"),
    "'by'.*subset"
  )

  # error for elements unquoted
  expect_error(
    sdc_descriptives(sdc_descriptives_DT, id, "val_1"),
    "object 'id' not found"
  )
  expect_error(
    sdc_descriptives(sdc_descriptives_DT, "id", val_1),
    "object 'val_1' not found"
  )

  # error for missing arguments
  expect_error(
    sdc_descriptives(id_var = "id", val_var = "val_1"),
    'argument "data" is missing, with no default',
    fixed = TRUE
  )
  expect_silent(
    sdc_descriptives(sdc_descriptives_DT, "id")
  )

  options(sdc.id_var = NULL)
  expect_error(
    sdc_descriptives(sdc_descriptives_DT, val_var = "val_1"),
    "Assertion on 'id_var' failed: Must be of type 'string', not 'NULL'.",
    fixed = TRUE
  )
})


# missing ID's ----
test_that("missing ID's are handled correctly (simple case)", {
  data("sdc_descriptives_DT")
  options(sdc.info_level = NULL)

  distinct_ids_ref_6 <- structure(
    data.table(distinct_ids = 8L),
    class = c("sdc_distinct_ids", "data.table", "data.frame")
  )
  dominance_ref_6 <- structure(
    data.table(value_share = 0.054822016086913394),
    class = c("sdc_dominance", "data.table", "data.frame")
  )
  descriptives_ref_6 <- structure(
    list(
      options = sdcLog:::list_options(),
      settings = sdcLog:::list_arguments("id_na", "val_1", zero_as_NA = FALSE),
      distinct_ids = distinct_ids_ref_6,
      dominance = dominance_ref_6
    ),
    class = c("sdc_descriptives", "list")
  )

  expect_equal(
    sdc_descriptives(sdc_descriptives_DT[seq(2, 20, 2)], "id_na", "val_1"),
    descriptives_ref_6,
    ignore_attr = TRUE
  )


  distinct_ids_ref_7 <- structure(
    data.table(distinct_ids = 9L),
    class = c("sdc_distinct_ids", "data.table", "data.frame")
  )
  dominance_ref_7 <- structure(
    data.table(value_share = 0.8714379273090671),
    class = c("sdc_dominance", "data.table", "data.frame")
  )
  descriptives_ref_7 <- structure(
    list(
      options = sdcLog:::list_options(),
      settings = sdcLog:::list_arguments("id_na", "val_1", zero_as_NA = FALSE),
      distinct_ids = distinct_ids_ref_7,
      dominance = dominance_ref_7
    ),
    class = c("sdc_descriptives", "list")
  )
  DT_filled <- copy(sdc_descriptives_DT)
  DT_filled[is.na(id_na), id_na := "X"]


  expect_warning(
    expect_equal(
      sdc_descriptives(DT_filled[seq(2, 20, 2)], "id_na", "val_1"),
      descriptives_ref_7,
      ignore_attr = TRUE
    ),
    paste(cli::style_bold("DISCLOSURE PROBLEM:"), "Dominant entities."),
    fixed = TRUE
  )
})

test_that("missing ID's are handled correctly (by case)", {
  data("sdc_descriptives_DT")
  options(sdc.info_level = 2)

  distinct_ids_ref_8 <- structure(
    data.table(
      sector = factor(c("S1", "S2")),
      distinct_ids = rep(4L, 2L)
    ),
    class = c("sdc_distinct_ids", "data.table", "data.frame")
  )
  dominance_ref_8 <- structure(
    data.table(
      sector = factor(c("S1", "S2")),
      value_share = c(0.12990197140157855, 0.08510201158264474)
    ),
    class = c("sdc_dominance", "data.table", "data.frame")
  )
  descriptives_ref_8 <- structure(
    list(
      options = sdcLog:::list_options(),
      settings = sdcLog:::list_arguments(
        "id_na", "val_1", by = "sector", zero_as_NA = FALSE
      ),
      distinct_ids = distinct_ids_ref_8,
      dominance = dominance_ref_8
    ),
    class = c("sdc_descriptives", "list")
  )


  expect_warning(
    expect_equal(
      sdc_descriptives(
        sdc_descriptives_DT[seq(2, 20, 2)],
        "id_na",
        "val_1",
        by = "sector"
      ),
      descriptives_ref_8,
      ignore_attr = TRUE
    ),
    paste(
        cli::style_bold("DISCLOSURE PROBLEM:"), "Not enough distinct entities."
    ),
    fixed = TRUE
  )


  distinct_ids_ref_9 <- structure(
    data.table(
      sector = factor(c("S1", "S2")),
      distinct_ids = rep(5L, 2L)
    ),
    class = c("sdc_distinct_ids", "data.table", "data.frame")
  )
  dominance_ref_9 <- structure(
    data.table(
      sector = factor(c("S2", "S1")),
      value_share = c(0.905631390, 0.877685169)
    ),
    class = c("sdc_dominance", "data.table", "data.frame")
  )
  descriptives_ref_9 <- structure(
    list(
      options = sdcLog:::list_options(),
      settings = sdcLog:::list_arguments(
        "id_na", "val_1", by = "sector", zero_as_NA = FALSE
      ),
      distinct_ids = distinct_ids_ref_9,
      dominance = dominance_ref_9
    ),
    class = c("sdc_descriptives", "list")
  )

  DT_filled <- copy(sdc_descriptives_DT)
  DT_filled[is.na(id_na), id_na := "X"]


  expect_warning(
    expect_equal(
      sdc_descriptives(DT_filled[seq(2, 20, 2)], "id_na", "val_1", by = "sector"),
      descriptives_ref_9,
      ignore_attr = TRUE
    ),
    paste(cli::style_bold("DISCLOSURE PROBLEM:"), "Dominant entities."),
    fixed = TRUE
  )
})

# no id's ----
test_that("all ID's NA are handled correctly", {
  data("sdc_descriptives_DT")
  sdc_descriptives_DT[, id_all_na := NA_character_]
  distinct_ids_ref_10 <- structure(
    data.table(
      sector = factor(character(), levels = c("S1", "S2")),
      distinct_ids = integer()
    ),
    class = c("sdc_distinct_ids", "data.table", "data.frame")
  )
  dominance_ref_10 <- structure(
    data.table(
      sector = factor(character(), levels = c("S1", "S2")),
      value_share = double()
    ),
    class = c("sdc_dominance", "data.table", "data.frame")
  )
  descriptives_ref_10 <- structure(
    list(
      options = sdcLog:::list_options(),
      settings = sdcLog:::list_arguments(
        "id_all_na", "val_1", by = "sector", zero_as_NA = FALSE
      ),
      distinct_ids = distinct_ids_ref_10,
      dominance = dominance_ref_10
    ),
    class = c("sdc_descriptives", "list")
  )

  expect_equal(
    sdc_descriptives(sdc_descriptives_DT, "id_all_na", "val_1", by = "sector"),
    descriptives_ref_10,
    ignore_attr = TRUE
  )
})

# some id's ----
test_that("argument fill_id_var works", {
    data("sdc_descriptives_DT")
    id_na <- sdc_descriptives_DT[["id_na"]]
    expect_warning(
        sdc_descriptives(sdc_descriptives_DT, "id_na", val_var = "val_1", by = "sector"),
        paste(
            cli::style_bold("DISCLOSURE PROBLEM:"), "Not enough distinct entities."
        ),
        fixed = TRUE
    )

    expect_silent(
        sdc_descriptives(sdc_descriptives_DT, "id_na", val_var = "val_1", by = "sector", fill_id_var = TRUE)
    )
    expect_identical(sdc_descriptives_DT[["id_na"]], id_na)
})


test_that("#77 is fixed", {
  options(sdc.info_level = 2)

  df <- data.table(id = "A", val = 1:4)
  descriptives_ref_issue_77_a <- structure(
    list(
      options = sdcLog:::list_options(),
      settings = sdcLog:::list_arguments(
        "id", "val", zero_as_NA = FALSE
      ),
      distinct_ids = structure(
        class = c("sdc_distinct_ids", "data.table", "data.frame"),
        data.table(distinct_ids = 1L)
      ),
      dominance = structure(
        class = c("sdc_dominance", "data.table", "data.frame"),
        data.table(value_share = 1L)
      )
    ),
    class = c("sdc_descriptives", "list")
  )

  warnings <- capture_warnings(
    expect_equal(
      sdc_descriptives(df, "id", val_var = "val"),
      descriptives_ref_issue_77_a,
      ignore_attr = TRUE
    )
  )
  expect_match(
    warnings,
    "DISCLOSURE PROBLEM:.*(Not enough distinct entities|Dominant entities)\\."
  )

  df[, id := NA_character_]
  descriptives_ref_issue_77_b <- structure(
    list(
      options = sdcLog:::list_options(),
      settings = sdcLog:::list_arguments(
        "id", "val", zero_as_NA = FALSE
      ),
      distinct_ids = structure(
        class = c("sdc_distinct_ids", "data.table", "data.frame"),
        data.table(distinct_ids = 0L)
      ),
      dominance = structure(
        class = c("sdc_dominance", "data.table", "data.frame"),
        data.table(value_share = double())
      )
    ),
    class = c("sdc_descriptives", "list")
  )

  expect_equal(
    sdc_descriptives(df, "id", val_var = "val"),
    descriptives_ref_issue_77_b,
    ignore_attr = TRUE
  )
})

test_that("#83 is fixed", {
  df <- data.table(
    id = c("N", NA, NA, NA, "N", "N"),
    by_var = factor(c("U", "U", "N", "M", "M", "N"), levels = c("U", "M", "N")),
    val = c(7, 2, 500, 3000, 4, 1)
  )

  expect_warning(
    {res <- sdc_descriptives(df, "id", "val", "by_var")},
    paste(
        cli::style_bold("DISCLOSURE PROBLEM:"), "Not enough distinct entities."
    ),
    fixed = TRUE
  )
  expect_equal(
    as.data.table(res[[4]]),
    data.table(
      by_var = factor(c("U", "N", "M"), levels = c("U", "M", "N")),
      value_share = c(0.555555556, 0.001996008, 0.001331558)
    )
  )
})

test_that("preventing val_var = 'val_var' works", {
  df <- data.table(id_var = "A", val_var = 1L)
  expect_error(
    sdc_descriptives(df, id_var = "id_var", val_var = "val_var"),
    "Assertion on 'val_var' failed: Must not equal \"val_var\".",
    fixed = TRUE
  )
})

