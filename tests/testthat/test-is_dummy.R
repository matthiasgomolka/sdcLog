# test setup ####
set.seed(1L)
n <- 5L

# create vectors
# testing for NA values is not necessary since any NA's are removed in
# sdc_model() before is_dummy() is called
integer <- seq_len(n)
numeric <- as.numeric(integer)
character <- as.character(integer)
factor <- as.factor(integer)
logical <- sample(c(TRUE, FALSE), n, replace = TRUE)
# dummy_int <- as.integer(logical)
# dummy_num <- as.numeric(logical)
fake_dummy <- as.integer(logical) + 1L
complex <- complex(n, real = integer, imaginary = integer)
list <- list(integer = integer, logical = logical)

# test is_dummy_col ----
# test that is_dummy_col returns always a logical
test_that("is_dummy() always returns a logical", {
  expect_type(is_dummy(integer), "logical")
  expect_type(is_dummy(numeric), "logical")
  expect_type(is_dummy(character), "logical")
  expect_type(is_dummy(factor), "logical")
  expect_type(is_dummy(logical), "logical")
  # expect_type(is_dummy(dummy_int), "logical")
  # expect_type(is_dummy(dummy_num), "logical")
  expect_type(is_dummy(fake_dummy), "logical")
  expect_type(is_dummy(complex), "logical")
  expect_type(is_dummy(list), "logical")
})

# test that is_dummy_col returns correct for different variables
test_that("is_dummy() classifies correctly", {
  expect_true(is_dummy(character), "logical")
  expect_true(is_dummy(factor), "logical")
  expect_true(is_dummy(logical), "logical")
  # expect_true(is_dummy(dummy_int), "logical")
  # expect_true(is_dummy(dummy_num), "logical")

  expect_false(is_dummy(integer), "logical")
  expect_false(is_dummy(numeric), "logical")
  expect_false(is_dummy(fake_dummy), "logical")
  expect_false(is_dummy(complex), "logical")
  expect_false(is_dummy(list), "logical")
})
