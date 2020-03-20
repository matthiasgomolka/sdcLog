library(data.table)
library(testthat)

set.seed(3)

# small test dt
extreme_test_dt <- data.table(
    id = rep_len(LETTERS[1L:10L], 10),
    val = c(10:1),
    val_2 = c(100, 90, 8:1),
    val_3 = c(NA, 9:1),
    key = "id"
)


# test sdc_extreme ----
context("sdc_extreme")

# test that sdc_extreme returns a data.table
test_that("sdc_extreme() returns a data.table", {
    expect_true(is.data.table(sdc_extreme(extreme_test_dt, "id", "val")))
})


## functionality tests

# create ref. 1
# extreme values for val_var = val
min_test_val <- extreme_test_dt[6:10, mean(val)]
max_test_val <- extreme_test_dt[1:5, mean(val)]

extreme_ref_1 <- data.table(val_var = "val",
                            min = min_test_val,
                            n_obs_min = 5,
                            max = max_test_val,
                            n_obs_max = 5
)


# create ref. 2
# extreme values for val_var = val_2, n_min/max = 5
min_test_val_2 <- extreme_test_dt[6:10, mean(val_2)]
# max_test_val_2 <- extreme_test_dt[1:5, mean(val_2)]
# problem with dominance (1:5), so 1:9 necessary
# max_test_val_2 <- extreme_test_dt[1:9, mean(val_2)]
# but would lead to overlap, so all extreme values NA:

extreme_ref_2 <- data.table(val_var = "val_2",
                            min = NA_real_,
                            n_obs_min = NA_integer_,
                            max = NA_real_,
                            n_obs_max = NA_integer_
)


# create ref. 3
# extreme values for val_var = val_3, n_min/max = 5
min_test_val_3 <- extreme_test_dt[6:10, mean(val_3)]
# max_test_val_3 <- extreme_test_dt[1:5, mean(val_3)]
# problem with NA in (1:5), so 2:6 necessary
# but would lead to overlap, so all extreme values NA:

extreme_ref_3 <- data.table(val_var = "val_3",
                            min = NA_real_,
                            n_obs_min = NA_integer_,
                            max = NA_real_,
                            n_obs_max = NA_integer_
)


# test that sdc_extreme() computes corretly
test_that("sdc_extreme() computes corretly", {
    expect_equal(
        sdc_extreme(extreme_test_dt, "id", "val"),
        extreme_ref_1
    )
    expect_equal(
        sdc_extreme(extreme_test_dt, "id", "val_2"),
        extreme_ref_2
    )
    expect_equal(
        sdc_extreme(extreme_test_dt, "id", "val_3"),
        extreme_ref_3
    )
  }
)



# setup sdc_extreme() returns correct messages

#1: val
extreme_expect_1 <- function(x) {
    messages <- capture_messages(x)
    expect_match(
        paste0(messages, collapse = ""),
        paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85 ]\n",
               "[ SETTINGS: id_var: id | val_var: val ]\n",
               collapse = ""),
        fixed = TRUE
    )
}

#2: val_2
extreme_expect_2 <- function(x) {
    messages <- capture_messages(x)
    expect_match(
        paste0(messages, collapse = ""),
        paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85 ]\n",
               "[ SETTINGS: id_var: id | val_var: val_2 ]\n",
               "It is impossible to compute extreme values for variable 'val_2' that comply to RDSC rules.",
               collapse = ""),
        fixed = TRUE
    )
}

#3: val_3
extreme_expect_3 <- function(x) {
    messages <- capture_messages(x)
    expect_match(
        paste0(messages, collapse = ""),
        paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85 ]\n",
               "[ SETTINGS: id_var: id | val_var: val_3 ]\n",
               "It is impossible to compute extreme values for variable 'val_3' that comply to RDSC rules.",
               collapse = ""),
        fixed = TRUE
    )
}

# test that sdc_extreme returns correct messages
test_that("sdc_extreme() returns correct messages", {
    extreme_expect_1(
        sdc_extreme(extreme_test_dt, "id", "val"))
    extreme_expect_2(
        sdc_extreme(extreme_test_dt, "id", "val_2"))
    extreme_expect_3(
        sdc_extreme(extreme_test_dt, "id", "val_3"))
})



# test dt für by
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
                            unique(extreme_test_dt_by[, "sector"]),
                            min = c(min_test_val_by_S1, min_test_val_by_S2),
                            n_obs_min = n_obs_test_val_by,
                            max = c(max_test_val_by_S1, max_test_val_by_S2),
                            n_obs_max = n_obs_test_val_by
)

# test that sdc_extreme computes correctly
test_that("sdc_extreme() computes correctly", {
    expect_equal(
        sdc_extreme(extreme_test_dt_by, "id", "val", by = "sector"),
        extreme_ref_4
    )
})


# test extreme values for val_var = val_2, by = sector
val_var_test_by <- "val_2"
by <- "sector"
data.table::setorderv(extreme_test_dt_by, cols = val_var_test_by, order = -1L)
n_obs_test_val_2_by <- 5

min_test_val_2_by_S1 <- extreme_test_dt_by[6:10, mean(val_2)]
min_test_val_2_by_S2 <- extreme_test_dt_by[16:20, mean(val_2)]

# max: problem with dominance (1:5), so 1:7 necessary:
# max_test_val_2_by_S1 <- extreme_test_dt_by[1:7, mean(val_2)]
# but would lead to overlap, so all extreme values NA for S1:
min_test_val_by_S1 <- NA_real_
max_test_val_by_S1 <- NA_real_
n_obs_test_val_2_by <- NA_integer_

# max S2
# max_test_val_by_S2 <- extreme_test_dt_by[11:15, mean(val)]
# würde funktioneren, aber dennoch NA für alle da keine Rückschlüsse?
min_test_val_by_S2 <- NA_real_
max_test_val_by_S2 <- NA_real_

extreme_ref_5 <- data.table(val_var = val_var_test_by,
                            unique(extreme_test_dt_by[, "sector"]),
                            min = c(min_test_val_by_S1, min_test_val_by_S2),
                            n_obs_min = n_obs_test_val_2_by,
                            max = c(max_test_val_by_S1, max_test_val_by_S2),
                            n_obs_max = n_obs_test_val_2_by
)

# test that sdc_extreme computes correctly
test_that("sdc_extreme() computes correctly", {
    expect_equal(
        sdc_extreme(extreme_test_dt_by, "id", "val_2", by = "sector"),
        extreme_ref_5
    )
})


## test that sdc_extreme returns correct messages with by argument

#set up: val, by = "sector"
extreme_expect_4 <- function(x) {
    messages <- capture_messages(x)
    expect_match(
        paste0(messages, collapse = ""),
        paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85 ]\n",
               "[ SETTINGS: id_var: id | val_var: val | by: sector ]\n",
               collapse = ""),
        fixed = TRUE
    )
}

#set up: val_2, by = "sector"
extreme_expect_5 <- function(x) {
    messages <- capture_messages(x)
    expect_match(
        paste0(messages, collapse = ""),
        paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85 ]\n",
               "[ SETTINGS: id_var: id | val_var: val_2 | by: sector ]\n",
               "It is impossible to compute extreme values for variable 'val_2' that comply to RDSC rules.",
               collapse = ""),
        fixed = TRUE
    )
}

# test that sdc_extreme returns correct messages with by argument
test_that("sdc_extreme() returns correct messages", {
    extreme_expect_4(
        sdc_extreme(extreme_test_dt_by, "id", "val", "sector"))
    extreme_expect_5(
        sdc_extreme(extreme_test_dt_by, "id", "val_2", "sector"))
})



# more tests für interne Funktionen etc.




