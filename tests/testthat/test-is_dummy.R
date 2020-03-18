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
complex <- c(1 + 4i, n)


# test is_dummy_col ----
context("is_dummy")

#test that is_dummy_col returns a logical for different variable types
test_that("is_dummy() returns a logical", {
    expect_type(sdcLog:::is_dummy(factor), "logical")
    expect_type(sdcLog:::is_dummy(numeric), "logical")
    expect_type(sdcLog:::is_dummy(character), "logical")
    expect_type(sdcLog:::is_dummy(integer), "logical")
    expect_type(sdcLog:::is_dummy(logical), "logical")
    expect_type(sdcLog:::is_dummy(dummy), "logical")
    expect_type(sdcLog:::is_dummy(fake_dummy), "logical")
    expect_type(sdcLog:::is_dummy(complex), "logical")
})

#test that is_dummy_col returns correct for different variables
test_that("is_dummy() returns correct", {
    expect_true(sdcLog:::is_dummy(logical))
    expect_true(sdcLog:::is_dummy(dummy))
    expect_true(sdcLog:::is_dummy(factor))
    expect_true(sdcLog:::is_dummy(character))
    expect_false(sdcLog:::is_dummy(numeric))
    expect_false(sdcLog:::is_dummy(integer))
    expect_false(sdcLog:::is_dummy(fake_dummy))
    expect_false(sdcLog:::is_dummy(complex))
})



