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
distinct_ids_ref_1_dt <- data.table(distinct_ids = 10L)
distinct_ids_ref_2_dt <- data.table(
  sector = c("S1", "S2"), distinct_ids = 5L, key = "sector"
)
distinct_ids_ref_3_dt <- data.table(
  sector = c("S1", "S1", "S2", "S2"),
  year = rep(2019L:2020L, 2L),
  distinct_ids = c(4L, rep(5L, 3L)),
  key = c("sector", "year")
)

# test check_dominance ----

## functionality tests
dominance_ref_1_dt <- data.table(value_share = 0.811146196943163)
dominance_ref_2_dt <- data.table(
  sector = c("S2", "S1"),
  value_share = c(0.888866740023071, 0.834414924858227)
)
dominance_ref_3_dt <- data.table(
  sector = c("S2", "S1", "S1", "S2"),
  year = c(rep(2020L, 2L), rep(2019L, 2L)),
  value_share = c(0.934568784234764, 0.913682312146633, 0.68150105511851, 0.550696457360742)
)


# test sdc_descriptives ####
# descriptives setup 1 ####
descriptives_ref_1 <- structure(
  list(
    message_options = sdcLog:::message_options(),
    message_arguments = sdcLog:::message_arguments("id", "val", zero_as_NA = FALSE),
    distinct_ids = structure(
      distinct_ids_ref_1_dt,
      class = c("sdc_distinct_ids", class(distinct_ids_ref_1_dt))
    ),
    dominance = structure(
      dominance_ref_1_dt,
      class = c("sdc_dominance", class(dominance_ref_1_dt))
    )
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
    distinct_ids = structure(
      distinct_ids_ref_2_dt,
      class = c("sdc_distinct_ids", class(distinct_ids_ref_2_dt))
    ),
    dominance = structure(
      dominance_ref_2_dt,
      class = c("sdc_dominance", class(dominance_ref_2_dt))
    )
  ),
  class = c("sdc_descriptives", "list")
)

descriptives_expect_2 <- function(x) {
  expect_warning(
    expect_equal(
      x,
      descriptives_ref_2
    ),
    paste0(crayon::bold("DISCLOSURE PROBLEM: "),
           "Dominant entities."),
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
    distinct_ids = structure(
      distinct_ids_ref_3_dt,
      class = c("sdc_distinct_ids", class(distinct_ids_ref_3_dt))
    ),
    dominance = structure(
      dominance_ref_3_dt,
      class = c("sdc_dominance", class(dominance_ref_3_dt))
    )
  ),
  class = c("sdc_descriptives", "list")
)

descriptives_expect_3 <- function(x) {
  warnings <- capture_warnings(
    expect_equal(
      x,
      descriptives_ref_3
    )
  )
  expect_match(
    warnings,
    "DISCLOSURE PROBLEM:.*(Not enough distinct entities|Dominant entities)\\."
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

options(sdc.info_level = 2)
descriptives_ref_4 <- structure(
  list(
    message_options = sdcLog:::message_options(),
    message_arguments = sdcLog:::message_arguments("id"),
    distinct_ids = structure(
      distinct_ids_ref_1_dt,
      class = c("sdc_distinct_ids", class(distinct_ids_ref_1_dt))
    ),
    dominance = structure(
      data.table(value_share = NA_real_),
      class = c("sdc_dominance", class(dominance_ref_1_dt))
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
    distinct_ids = structure(
      distinct_ids_ref_1_dt,
      class = c("sdc_distinct_ids", class(distinct_ids_ref_1_dt))
    ),
    dominance = structure(
      dominance_ref_1_dt,
      class = c("sdc_dominance", class(dominance_ref_1_dt))
    )
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

# add tests for expressions in 'by'
