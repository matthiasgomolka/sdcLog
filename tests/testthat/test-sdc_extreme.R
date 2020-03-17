library(data.table)
library(testthat)

set.seed(3)

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


# test sdc_extreme ----
context("sdc_extreme")

#test that sdc_extreme returns a data.table
test_that("sdc_extreme() returns a data.table", {
    expect_true(is.data.table(sdc_extreme(test_dt, "id", "val")))
})

# more tests will be added


