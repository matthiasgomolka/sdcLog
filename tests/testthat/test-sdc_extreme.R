library(data.table)
library(testthat)

set.seed(1)

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



# test dt für by argument
n <- 20
extreme_test_dt_by <- data.table(
    id = rep_len(LETTERS[1L:10L], n),
    val = c(20:1),
    sector = sort(rep_len(paste0("S", 1L:2L), n)),
    val_2 = c(200, 190, 18:1),
    val_3 = c(NA, 19:1),
    val_4 = c(20:7, rep(NA, 6)),
    key = "id"
)

# setup test extreme values for val_var = val, by = sector
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


# setup test extreme values for val_var = val_2, by = sector
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

# test that sdc_extreme computes correctly with by argument
test_that("sdc_extreme() computes correctly", {
    expect_equal(
        sdc_extreme(extreme_test_dt_by, "id", "val", by = "sector"),
        extreme_ref_4
        )
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



# interne Funktionen:

#find_SD_problems:
context("find_SD_problems")

# test that find_SD_problems returns a list
test_that("find_SD_problems() returns a list", {
    expect_true(is.list(find_SD_problems(extreme_test_dt, head, 5, "id", "val")))
    expect_true(is.list(find_SD_problems(extreme_test_dt, tail, 5, "id", "val")))
    }
)

# test that find_SD_problems detects problems corretly

# set up:
# case 1: extreme_test_dt: no problems with subset of head/tail 5 obs. for val_var = val
    # so list[["problems]] == FALSE
# case 2: extreme_test_dt: problems with subset of head 5 obs. for val_var = val_2, no problems for tail
    # so list[["problems"]] == TRUE for head, for tail == FALSE
# case 3: extreme_test_dt: no problems with subset of head/tail 5 obs. for val_var = val
    # so list[["problems]] == FALSE
    # but NA's have to be excluded the same way as in function sdc_extreme:
    case_3 <- na.omit(extreme_test_dt, cols = "val_3")
# case 4: extreme_test_dt_by: no problems with subset of head/tail 5 obs. for val_var = val and by = "sector"
    # so list[["problems]] == FALSE
# case 5: extreme_test_dt_by: problems with subset of head 5 obs. for val_var = val_2 and by = "sector", no problems for tail
    # so list[["problems"]] == TRUE for head, for tail == FALSE
# case 6: extreme_test_dt_by: problems with subset of head & tail 5 obs. for val_var = val_4 and by = "sector"
    # so list[["problems"]] == TRUE
    # but NA's have to be excluded the same way as in function sdc_extreme:
    case_6 <- na.omit(extreme_test_dt_by, cols = "val_4")

test_that("find_SD_problems() detects problems corretly", {
    # case 1
    expect_equal(find_SD_problems(extreme_test_dt, head, 5, "id", "val")[["problems"]], FALSE)
    expect_equal(find_SD_problems(extreme_test_dt, tail, 5, "id", "val")[["problems"]], FALSE)

    # case 2
    expect_equal(find_SD_problems(extreme_test_dt, head, 5, "id", "val_2")[["problems"]], TRUE)
    expect_equal(find_SD_problems(extreme_test_dt, tail, 5, "id", "val_2")[["problems"]], FALSE)

    # case 3
    expect_equal(find_SD_problems(case_3, head, 5, "id", "val_3")[["problems"]], FALSE)
    expect_equal(find_SD_problems(case_3, tail, 5, "id", "val_3")[["problems"]], FALSE)

    # case 4
    expect_equal(find_SD_problems(extreme_test_dt_by, head, 5, "id", "val", "sector")[["problems"]], FALSE)
    expect_equal(find_SD_problems(extreme_test_dt_by, tail, 5, "id", "val", "sector")[["problems"]], FALSE)

    # case 5
    expect_equal(find_SD_problems(extreme_test_dt_by, head, 5, "id", "val_2", "sector")[["problems"]], TRUE)
    expect_equal(find_SD_problems(extreme_test_dt_by, tail, 5, "id", "val_2", "sector")[["problems"]], FALSE)

    # case 6
    expect_equal(find_SD_problems(case_6, head, 5, "id", "val_4", "sector")[["problems"]], TRUE)
    expect_equal(find_SD_problems(case_6, tail, 5, "id", "val_4", "sector")[["problems"]], TRUE)
}
)


#find SD problems:
context("find_SD")

# test that find_SD returns a data.table
test_that("find_SD() returns a data.table", {
    expect_true(is.data.table(find_SD(extreme_test_dt, "min", 5, "id", "val")))
    expect_true(is.data.table(find_SD(extreme_test_dt, "max", 5, "id", "val")))
}
)

# test that find_SD returns correct subset

# setup 1: subset for val_var = val
# no problems at all, so subset max: 1:5, subset min: 6:10
test_1_results_min <- extreme_test_dt[6:10, ]
test_1_results_max <- extreme_test_dt[1:5, ]

# setup 2: subset for val_var = val_2
# problems with dominance for max, so subset max: 1:9, subset min: 6:10
test_2_results_min <- extreme_test_dt[6:10, ]
test_2_results_max <- extreme_test_dt[1:9, ]

# setup 3: subset for val_var = "val" and by = "sector"
# no problems at all, so subsets with different col order
test_3_results_max <- extreme_test_dt_by[c(1:5, 11:15), c(3, 1, 2, 4, 5, 6)]
test_3_results_min <- extreme_test_dt_by[c(6:10, 16:20), c(3, 1, 2, 4, 5, 6)]


test_that("find_SD() returns correct subset", {
    expect_equal(find_SD(extreme_test_dt, "min", 5, "id", "val"), test_1_results_min)
    expect_equal(find_SD(extreme_test_dt, "max", 5, "id", "val"), test_1_results_max)

    expect_equal(find_SD(extreme_test_dt, "min", 5, "id", "val"), test_2_results_min)
    expect_equal(find_SD(extreme_test_dt, "max", 9, "id", "val"), test_2_results_max)

    expect_equal(find_SD(extreme_test_dt_by, "min", 5, "id", "val", "sector"), test_3_results_min)
    expect_equal(find_SD(extreme_test_dt_by, "max", 5, "id", "val", "sector"), test_3_results_max)
}
)






