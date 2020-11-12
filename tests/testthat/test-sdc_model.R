library(data.table)

# create dt for model tests ----
set.seed(1)
n <- 80

y <- rnorm(n, mean = 120, sd = 8)
model_test_dt <- data.table(
  id = rep_len(LETTERS[1L:10L], n),
  y = y,
  x_1 = jitter(y, factor = 10000),
  x_2 = jitter(y, factor = 15000),
  x_3 = jitter(y, factor = 50000),
  dummy_1 = sort(rep_len(paste0("M", 1L:2L), n)),
  dummy_2 = as.factor(rep_len(paste0("Y", 1L:8L), n)),
  dummy_3 = c(
    rep(rawToChar(as.raw(c(66, 69))), n / 4),
    rep(rawToChar(as.raw(68:69)), n / 4),
    rep(rawToChar(as.raw(c(69, 83))), 36),
    rep(rawToChar(as.raw(c(70, 82))), 4)
  ),
  key = "id"
)


# create problems id's for x_3
model_test_dt[id %chin% c("A", "B", "C", "D", "E", "F"), x_3 := NA_real_]

# characteristics variables:
# id: id variable
# y: dependent variable
# independent variables:
# x_1 & x_2: should lead to no problems at all in model
# x_3: leads in model to problems with distinct id's
# dummy_1 & dummy_2: lead in model to no problems at all
# dummy_3: leads in model to probelms with dummy variable


# set up: models ----

# model 1:
# all good, no problems at all
# y = β0 + β1*x_1 + β2*x_2 + u
model_1 <- lm(y ~ x_1 + x_2, data = model_test_dt)
summary(model_1)

# sdc_model(model_test_dt, model_1, "id")

# model 2:
# problem distinct id's, no dummys
# y = β0 + β1*x_1 + β2*x_2 + β3*x_3 + u
model_2 <- lm(y ~ x_1 + x_2 + x_3, data = model_test_dt)
summary(model_2)

# sdc_model(model_test_dt, model_2, "id")


# model 4:
# all good, with dummys
# y = β0 + β1*x_1 + β2*x_2 + β3*dummy_1 + β4*dummy_2 + u
model_3 <- lm(y ~ x_1 + x_2 + dummy_1 + dummy_2, data = model_test_dt)
summary(model_3)

# sdc_model(model_test_dt, model_3, "id")


# model 5:
# only problems with dummy (dummy_3)
# y = β0 + β1*x_1 + β2*x_2 + β3*dummy_3 + u
model_4 <- lm(y ~ x_1 + x_2 + dummy_3, data = model_test_dt)
summary(model_4)

# sdc_model(model_test_dt, model_4, "id")

# tests: return, functionality etc.

# test sdc_model ----
# functionality tests

# test that sdc_model() returns warnings, if necessary
# warnings expected:
# model 2 (distinct id's problem),
# model 3 (dominance problem)
# model 5 (dummy problem)

test_that("sdc_model() returns warning, if necessary", {
  capture.output(
    expect_warning(sdc_model(model_test_dt, model_2, "id"))
  )
  capture.output(
    expect_warning(sdc_model(model_test_dt, model_4, "id"))
  )
})


# sdc_model() works/returns correctly

### set up model_1:
# y = β0 + β1*x_1 + β2*x_2 + u
# no problems at all
# create distinct ref
distinct_ref_1 <- data.table(distinct_ids = 10L)
class(distinct_ref_1) <- c("sdc_distinct_ids", class(distinct_ref_1))

# create dummy list ref
dummy_ref_1 <- list()
dummy_vars <- as.character()
names(dummy_ref_1) <- dummy_vars

# create ref. list
res_1 <- list(
  message_options = message_options(),
  message_arguments = message_arguments(id_var = "id"),
  distinct_ids = distinct_ref_1,
  dummies = dummy_ref_1
)

class(res_1) <- c("sdc_model", class(res_1))

# test that sdc_model works correctly
test_that("sdc_model() returns/works correctly", {
  expect_equal(sdc_model(model_test_dt, model_1, "id"), res_1)
})

### set up model_2:
# y = β0 + β1*x_1 + β2*x_2 + β3*x_3 + u
# problems distinct id's
# create distinct ref
distinct_ref_2 <- data.table(distinct_ids = 4L)
class(distinct_ref_2) <- c("sdc_distinct_ids", class(distinct_ref_2))

# create dummy list ref
dummy_ref_2 <- list()
dummy_vars <- as.character()
names(dummy_ref_2) <- dummy_vars

# create ref. list
res_2 <- list(
  message_options = message_options(),
  message_arguments = message_arguments(id_var = "id"),
  distinct_ids = distinct_ref_2,
  dummies = dummy_ref_2
)

class(res_2) <- c("sdc_model", class(res_2))

# test that sdc_model works correctly
test_that("sdc_model() returns/works correctly", {
  expect_warning(
    capture_output(
      expect_equal(sdc_model(model_test_dt, model_2, "id"), res_2)
    ),
    "Potential disclosure problem: Not enough distinct entities."
  )
})


### set up model_3:
# y = β0 + β1*x_1 + β2*x_2 + β3*dummy_1 + β4*dummy_2 + u
# all good, with dummys
# create distinct ref
distinct_ref_3 <- data.table(distinct_ids = 10L)
class(distinct_ref_3) <- c("sdc_distinct_ids", class(distinct_ref_3))

# create dummy list ref
dummy_1 <- data.table(
  dummy_1 = c("M1", "M2"),
  distinct_ids = 10L
)

dummy_2 <- data.table(
  dummy_2 = factor(paste0("Y", 1:8)),
  distinct_ids = 5L
)

dummy_ref_3 <- list(dummy_1, dummy_2)
dummy_vars_3 <- c("dummy_1", "dummy_2")
names(dummy_ref_3) <- dummy_vars_3

# create ref. list
res_3 <- list(
  message_options = message_options(),
  message_arguments = message_arguments(id_var = "id"),
  distinct_ids = distinct_ref_3,
  dummies = dummy_ref_3
)
class(res_3) <- c("sdc_model", class(res_3))

# test that sdc_model works correctly
# check with equivalent (otherwise attributes (key) for dummys would have to be
# set)
test_that("sdc_model() returns/works correctly", {
  expect_equal(
    sdc_model(model_test_dt, model_3, "id"),
    res_3,
    ignore_attr = TRUE
  )
})


### set up model_4:
# y = β0 + β1*x_1 + β2*x_2 + β3*dummy_3 + u
# only problems with dummy_3
# create distinct ref
distinct_ref_4 <- data.table(distinct_ids = 10L)
class(distinct_ref_4) <- c("sdc_distinct_ids", class(distinct_ref_4))

# create dummy list ref
dummy_3 <- data.table(
  dummy_3 = c("FR", "BE", "DE", "ES"),
  distinct_ids = c(4L, rep(10L, 3L))
)
class(dummy_3) <- c("sdc_distinct_ids", class(dummy_3))

dummy_ref_4 <- list(dummy_3)
dummy_vars_4 <- c("dummy_3")
names(dummy_ref_4) <- dummy_vars_4

# create ref. list
res_4 <- list(
  message_options = message_options(),
  message_arguments = message_arguments(id_var = "id"),
  distinct_ids = distinct_ref_4,
  dummies = dummy_ref_4
)
class(res_4) <- c("sdc_model", class(res_4))

# test that sdc_model works correctly
# check with equivalent (otherwise attributes for dummys would have to be set)
test_that("sdc_model() returns/works correctly", {
  expect_warning(
    capture_output(
      expect_equal(
        sdc_model(model_test_dt, model_4, "id"),
        res_4
      )
    ),
    "Potential disclosure problem: Not enough distinct entities."
  )
})


# test arguments in sdc_model

# test that sdc_model retruns appropriate error
test_that("sdc_model() returns appropriate error", {

  # error für nichtexistierende Elemente
  expect_warning(
    expect_error(
      sdc_model(model_test_dt, wrong_model, "id"),
      "object 'wrong_model' not found"
    ),
    "restarting interrupted promise evaluation"
  )
  expect_error(
    sdc_model(model_test_dt, model_1, wrong_id),
    "object 'wrong_id' not found"
  )
  expect_error(
    sdc_model(wrong_model_dt, model_1, "id"),
    "object 'wrong_model_dt' not found"
  )

  # error für id_var unquoted
  expect_error(sdc_model(model_test_dt, model_1, id), "object 'id' not found")

  # error für data quoted
  expect_error(
    sdc_model("model_test_dt", model_1, "id"),
    "Assertion on 'data' failed: Must be of type 'data.frame', not 'character'."
  )

  # error für missing arguments
  expect_error(
    sdc_model(model_test_dt, model_1),
    "argument \"id_var\" is missing, with no default"
  )
  expect_error(
    sdc_model(model_test_dt, id_var = "id"),
    "argument \"model\" is missing, with no default"
  )
  expect_error(
    sdc_model(model = model_1, id_var = "id"),
    "argument \"data\" is missing, with no default"
  )
})
