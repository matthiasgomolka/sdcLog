# test setup ####
library(data.table)

set.seed(2)
n <- 80L


#create vectors
year <- as.factor(sort(rep_len(2018L:2020L, n)))
val <- runif(n, min = 1, max = 10)
sector <- sort(rep_len(paste0("S", 1L:4L), n))
y <- runif(n, min = 1, max = 100)
europe <- sample(c(TRUE, FALSE), n, replace = TRUE)
dummy <- sample(c(1, 0), n, replace = TRUE)



# test sdc_find_dummy_cols ----
context("sdc_find_dummy_cols")

#test that sdc_find_dummy_cols returns a logical for different variable types
test_that("sdc_find_dummy_cols() returns a logical", {
    expect_true(is.logical(sdc_find_dummy_cols(y)))
    expect_type(sdc_find_dummy_cols(y), "logical")
})

test_that("sdc_find_dummy_cols() returns a logical", {
    expect_true(is.logical(sdc_find_dummy_cols(dummy)))
    expect_type(sdc_find_dummy_cols(dummy), "logical")
})

test_that("sdc_find_dummy_cols() returns a logical", {
    expect_true(is.logical(sdc_find_dummy_cols(europe)))
    expect_type(sdc_find_dummy_cols(europe), "logical")
})

test_that("sdc_find_dummy_cols() returns a logical", {
    expect_true(is.logical(sdc_find_dummy_cols(sector)))
    expect_type(sdc_find_dummy_cols(sector), "logical")
})

test_that("sdc_find_dummy_cols() returns a logical", {
    expect_true(is.logical(sdc_find_dummy_cols(year)))
    expect_type(sdc_find_dummy_cols(year), "logical")
})


#test that sdc_find_dummy_cols returns right for different variables
test_that("sdc_find_dummy_cols() returns a logical", {
    expect_false(sdc_find_dummy_cols(y))
    expect_identical(sdc_find_dummy_cols(y), FALSE)
})

test_that("sdc_find_dummy_cols() returns a logical", {
    expect_true(sdc_find_dummy_cols(dummy))
    expect_identical(sdc_find_dummy_cols(dummy), TRUE)
})

test_that("sdc_find_dummy_cols() returns a logical", {
    expect_true(is.logical(sdc_find_dummy_cols(europe)))
    expect_identical(sdc_find_dummy_cols(europe), TRUE)
})

test_that("sdc_find_dummy_cols() returns a logical", {
    expect_true(is.logical(sdc_find_dummy_cols(sector)))
    expect_identical(sdc_find_dummy_cols(sector), TRUE)
})

test_that("sdc_find_dummy_cols() returns a logical", {
    expect_true(sdc_find_dummy_cols(year))
    expect_identical(sdc_find_dummy_cols(year), TRUE)
})

