library(data.table)
library(testthat)

set.seed(3)

# Test dt for other tests?
n <- 40L

test_dt <- data.table(
    id = rep_len(LETTERS[1L:10L], n),
    year = sort(rep_len(2019L:2020L, n)),
    val = runif(n, min = 1, max = 10),
    val_2 = runif(n, min = 1, max = 100),
    key = "id"
 )
 test_dt[, sector := sort(rep_len(paste0("S", 1L:2L), n))]
 test_dt[id == "A" & year == 2019L, val := NA_real_]
 test_dt[id %chin% c("A", "F") & year == 2020L, val := val * 50]
 setcolorder(test_dt, c("id", "sector", "year"))




# small test dt
extreme_test_dt <- data.table(
    id = rep_len(LETTERS[1L:10L], 10),
    val = c(10:1),
    val_2 = c(100, 90, 8:1),
    val_3 = c(NA, 9:1),
    key = "id"
)

# check sdc_extreme + interne Funktionen
# test sdc_extreme ----
context("sdc_extreme")

# test that sdc_extreme returns a data.table
test_that("sdc_extreme() returns a data.table", {
    expect_true(is.data.table(sdc_extreme(extreme_test_dt, "id", "val")))
})


## functionality tests
# test extreme values for val_var = val
val_var_test <- "val"
min_test_val <- extreme_test_dt[6:10, mean(val)]
n_obs_min_test_val <- 5
max_test_val <- extreme_test_dt[1:5, mean(val)]
n_obs_max_test_val <- 5

extreme_ref_1 <- data.table(val_var = val_var_test,
                            min = min_test_val,
                            n_obs_min = n_obs_min_test_val,
                            max = max_test_val,
                            n_obs_max = n_obs_max_test_val
)

test_that("sdc_extreme() computes correctly", {
    expect_equal(
        sdc_extreme(extreme_test_dt, "id", "val"),
        extreme_ref_1
    )
})

# test extreme values for val_var = val_2
val_2_var_test <- "val_2"
min_test_val_2 <- extreme_test_dt[6:10, mean(val_2)]
n_obs_min_test_val_2 <- 5
# problem with dominance (1:5), so 1:9 necessary
# max_test_val <- extreme_test_dt[1:9, mean(val_2)]
# but would lead to overlap, so all extreme values NA:
min_test_val_2 <- NA_real_
n_obs_min_test_val_2 <- NA_real_
max_test_val_2 <- NA_real_
n_obs_max_test_val_2 <- NA_real_

extreme_ref_2 <- data.table(val_var = val_2_var_test,
                            min = min_test_val_2,
                            n_obs_min = n_obs_min_test_val_2,
                            max = max_test_val_2,
                            n_obs_max = n_obs_max_test_val_2
)

test_that("sdc_extreme() computes correctly", {
    expect_equal(
        sdc_extreme(extreme_test_dt, "id", "val_2"),
        extreme_ref_2
    )
})


# test extreme values for val_var = val_3
val_3_var_test <- "val_3"
min_test_val_3 <- extreme_test_dt[6:10, mean(val_3)]
n_obs_min_test_val_3 <- 5
# problem with NA in (1:5), so 2:6 necessary
# max_test_val <- extreme_test_dt[2:6, mean(val_3)]
# but would lead to overlap, so all extreme values NA:
min_test_val_3 <- NA_real_
n_obs_min_test_val_3 <- NA_real_
max_test_val_3 <- NA_real_
n_obs_max_test_val_3 <- NA_real_

extreme_ref_3 <- data.table(val_var = val_3_var_test,
                            min = min_test_val_3,
                            n_obs_min = n_obs_min_test_val_3,
                            max = max_test_val_3,
                            n_obs_max = n_obs_max_test_val_3
)

test_that("sdc_extreme() computes correctly", {
    expect_equal(
        sdc_extreme(extreme_test_dt, "id", "val_3"),
        extreme_ref_3
    )
})

# test für by
n <- 20
extreme_test_dt_by <- data.table(
    id = rep_len(LETTERS[1L:10L], n),
    val = c(20:1),
    sector = sort(rep_len(paste0("S", 1L:2L), n)),
    val_2 = c(200, 190, 18:1),
    val_3 = c(NA, 19:1),
    key = "id"
)

# test extreme values for val_var = val, by = sector
val_var_test_by <- "val"
by <- "sector"
data.table::setorderv(extreme_test_dt_by, cols = val_var_test_by, order = -1L)
min_test_val_by_S1 <- extreme_test_dt_by[6:10, mean(val)]
n_obs_test_val_by <- 5
min_test_val_by_S2 <- extreme_test_dt_by[16:20, mean(val)]
max_test_val_by_S1 <- extreme_test_dt_by[1:5, mean(val)]
max_test_val_by_S2 <- extreme_test_dt_by[11:15, mean(val)]


extreme_ref_4 <- data.table(val_var = val_var_test_by,
                            by = by,
                            min = c(min_test_val_by_S1, min_test_val_by_S2),
                            n_obs_min = n_obs_test_val_by,
                            max = c(max_test_val_by_S1, max_test_val_by_S2),
                            n_obs_max = n_obs_test_val_by
)

test_that("sdc_extreme() computes correctly", {
    expect_equal(
        sdc_extreme(extreme_test_dt, "id", "val"),
        extreme_ref_1
    )
})


sdc_extreme(test_dt, "id", "val", by = "sector")
# just as character
sdc_extreme(test_dt, "id", "val", by = sector)

sdc_extreme(test_dt, "id", "val_2", by = "sector")


by <- NULL




# add tests für interne Funktionen








