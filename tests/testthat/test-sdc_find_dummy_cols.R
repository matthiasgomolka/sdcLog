# test setup ####
library(data.table)
library(testthat)

set.seed(2)
n <- 10L

#create vectors
factor <- as.factor(sort(rep_len(2018L:2020L, n)))
numeric <- runif(n, min = 1, max = 10)
character <- sort(rep_len(paste0("S", 1L:4L), n))
integer <- c(1:10)
logical <- sample(c(TRUE, FALSE), n, replace = TRUE)
dummy <- sample(c(1, 0), n, replace = TRUE)
fake_dummy <- sample(c(2, 3), n, replace = TRUE)
complex <- c(1+4i, n)


# test sdc_find_dummy_cols ----
context("sdc_find_dummy_cols")

#test that sdc_find_dummy_cols returns a logical for different variable types
test_that("sdc_find_dummy_cols() returns a logical", {
    expect_true(is.logical(sdc_find_dummy_cols(factor)))
    expect_true(is.logical(sdc_find_dummy_cols(numeric)))
    expect_true(is.logical(sdc_find_dummy_cols(character)))
    expect_true(is.logical(sdc_find_dummy_cols(integer)))
    expect_true(is.logical(sdc_find_dummy_cols(logical)))
    expect_true(is.logical(sdc_find_dummy_cols(dummy)))
    expect_true(is.logical(sdc_find_dummy_cols(fake_dummy)))
    expect_true(is.logical(sdc_find_dummy_cols(complex)))
})

#test that sdc_find_dummy_cols returns correct for different variables
test_that("sdc_find_dummy_cols() returns correct", {
    expect_true(sdc_find_dummy_cols(logical))
    expect_true(sdc_find_dummy_cols(dummy))
    expect_true(sdc_find_dummy_cols(factor))
    expect_true(sdc_find_dummy_cols(character))
    expect_false(sdc_find_dummy_cols(numeric))
    expect_false(sdc_find_dummy_cols(integer))
    expect_false(sdc_find_dummy_cols(fake_dummy))
    expect_false(sdc_find_dummy_cols(complex))
})



