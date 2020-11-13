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
# dummy_3: leads in model to problems with dummy variable

# model 1 ----
# all good, no problems at all
# y = β0 + β1*x_1 + β2*x_2 + u
model_1 <- lm(y ~ x_1 + x_2, data = model_test_dt)
summary(model_1)

# create ref
distinct_ref_1 <- structure(
  data.table(distinct_ids = 10L),
  class = c("sdc_distinct_ids", "data.table", "data.frame")
)
dummy_ref_1 <- structure(list(), names = character())
interactions_ref_1 <- structure(list(), names = character())

# create ref. list
res_1 <- structure(
  list(
    message_options = message_options(),
    message_arguments = message_arguments(id_var = "id"),
    distinct_ids = distinct_ref_1,
    dummies = dummy_ref_1,
    interactions = interactions_ref_1
  ),
  class = c("sdc_model", "list")
)

# test that sdc_model works correctly
test_that("sdc_model() returns/works correctly", {
  expect_equal(sdc_model(model_test_dt, model_1, "id"), res_1)
})

# model 2 ----
# problem distinct id's, no dummys
# y = β0 + β1*x_1 + β2*x_2 + β3*x_3 + u
model_2 <- lm(y ~ x_1 + x_2 + x_3, data = model_test_dt)
summary(model_2)

# create ref
distinct_ref_2 <- structure(
  data.table(distinct_ids = 4L),
  class = c("sdc_distinct_ids", "data.table", "data.frame")
)
dummy_ref_2 <- structure(list(), names = character())
interactions_ref_2 <- structure(list(), names = character())

# create ref. list
res_2 <- structure(
  list(
    message_options = message_options(),
    message_arguments = message_arguments(id_var = "id"),
    distinct_ids = distinct_ref_2,
    dummies = dummy_ref_2,
    interactions = interactions_ref_2
  ),
  class = c("sdc_model", "list")
)

# test that sdc_model works correctly
test_that("sdc_model() returns/works correctly", {
  expect_warning(
    capture_output(
      expect_equal(sdc_model(model_test_dt, model_2, "id"), res_2)
    ),
    "Potential disclosure problem: Not enough distinct entities."
  )
})


# model 3 ----
# all good, with dummys
# y = β0 + β1*x_1 + β2*x_2 + β3*dummy_1 + β4*dummy_2 + u
model_3 <- lm(y ~ x_1 + x_2 + dummy_1 + dummy_2, data = model_test_dt)
summary(model_3)

# create ref
distinct_ref_3 <- structure(
  data.table(distinct_ids = 10L),
  class = c("sdc_distinct_ids", "data.table", "data.frame")
)
dummy_ref_3 <- structure(
  list(
    structure(
      data.table(
        dummy_1 = c("M1", "M2"),
        distinct_ids = 10L
      ),
      class = c("sdc_distinct_ids", "data.table", "data.frame"),
      sorted = "dummy_1"
    ),
    structure(
      data.table(
        dummy_2 = factor(paste0("Y", 1:8)),
        distinct_ids = 5L
      ),
      class = c("sdc_distinct_ids", "data.table", "data.frame"),
      sorted = "dummy_2"
    )
  ),
  names = c("dummy_1", "dummy_2")
)
interactions_ref_3 <- structure(list(), names = character())

# create ref. list
res_3 <- structure(
  list(
    message_options = message_options(),
    message_arguments = message_arguments(id_var = "id"),
    distinct_ids = distinct_ref_3,
    dummies = dummy_ref_3,
    interactions = interactions_ref_3
  ),
  class = c("sdc_model", "list")
)

# test that sdc_model works correctly
# check with equivalent (otherwise attributes (key) for dummys would have to be
# set)
test_that("sdc_model() returns/works correctly", {
  expect_identical(
    sdc_model(model_test_dt, model_3, "id"),
    res_3
  )
})


# model 4 ----
# only problems with dummy (dummy_3)
# y = β0 + β1*x_1 + β2*x_2 + β3*dummy_3 + u
model_4 <- lm(y ~ x_1 + x_2 + dummy_3, data = model_test_dt)
summary(model_4)

# create ref
distinct_ref_4 <- structure(
  data.table(distinct_ids = 10L),
  class = c("sdc_distinct_ids", "data.table", "data.frame")
)
dummy_ref_4 <- structure(
  list(
    structure(
      data.table(
        dummy_3 = c("FR", "BE", "DE", "ES"),
        distinct_ids = c(4L, rep(10L, 3L))
      ),
      class = c("sdc_distinct_ids", "data.table", "data.frame")
      # not sorted, because ?
    )
  ),
  names = "dummy_3"
)
interactions_ref_4 <- structure(list(), names = character())

# create ref. list
res_4 <- list(
  message_options = message_options(),
  message_arguments = message_arguments(id_var = "id"),
  distinct_ids = distinct_ref_4,
  dummies = dummy_ref_4,
  interactions = interactions_ref_4
)
class(res_4) <- c("sdc_model", class(res_4))

# test that sdc_model works correctly
# check with equivalent (otherwise attributes for dummys would have to be set)
test_that("sdc_model() returns/works correctly", {
  expect_warning(
    capture_output(
      expect_identical(
        sdc_model(model_test_dt, model_4, "id"),
        res_4
      )
    ),
    "Potential disclosure problem: Not enough distinct entities."
  )
})


# model 5 ----
# interaction without problems
# y = β0 + β1*dummy_1 + β2*dummy_2 + β3*dummy_1*dummy_2 + u
model_5 <- lm(y ~ dummy_1 * dummy_2, data = model_test_dt)
summary(model_5)

# create ref
distinct_ref_5 <- structure(
  data.table(distinct_ids = 10L),
  class = c("sdc_distinct_ids", "data.table", "data.frame")
)
dummy_ref_5 <- structure(
  list(
    structure(
      data.table(
        dummy_1 = c("M1", "M2"),
        distinct_ids = 10L
      ),
      class = c("sdc_distinct_ids", "data.table", "data.frame"),
      sorted = "dummy_1"
    ),
    structure(
      data.table(
        dummy_2 = factor(paste0("Y", 1:8)),
        distinct_ids = 5L
      ),
      class = c("sdc_distinct_ids", "data.table", "data.frame"),
      sorted = "dummy_2"
    )
  ),
  names = c("dummy_1", "dummy_2")
)
inter_dt <- CJ(dummy_1 = c("M1", "M2"), dummy_2 = paste0("Y", 1:8)
)[, `:=`(
  `dummy_1:dummy_2` = paste(dummy_1, dummy_2, sep = ":"),
  distinct_ids = 5L
)][, c("dummy_1", "dummy_2") := NULL]
interactions_ref_5 <- structure(
  list(structure(
    inter_dt,
    class = c("sdc_distinct_ids", "data.table", "data.frame"),
    sorted = "dummy_1:dummy_2"
  )),
  names = "dummy_1:dummy_2"
)

# create ref. list
res_5 <- structure(
  list(
    message_options = message_options(),
    message_arguments = message_arguments(id_var = "id"),
    distinct_ids = distinct_ref_5,
    dummies = dummy_ref_5,
    interactions = interactions_ref_5
  ),
  class = c("sdc_model", "list")
)

# test that sdc_model works correctly
# check with equivalent (otherwise attributes for dummys would have to be set)
test_that("sdc_model() returns/works correctly", {
  expect_identical(
    sdc_model(model_test_dt, model_5, "id"),
    res_5
  )
})

# model 6 ----
# interaction with problems
# y = β0 + β1*dummy_1 + β2*dummy_2 + β3*dummy_1*dummy_2 + u
model_6 <- lm(y ~ dummy_1 : dummy_2 : dummy_3, data = model_test_dt)
summary(model_6)

# create ref
distinct_ref_6 <- structure(
  data.table(distinct_ids = 10L),
  class = c("sdc_distinct_ids", "data.table", "data.frame")
)
dummy_ref_6 <- structure(
  list(
    structure(
      data.table(
        dummy_1 = c("M1", "M2"),
        distinct_ids = 10L
      ),
      class = c("sdc_distinct_ids", "data.table", "data.frame"),
      sorted = "dummy_1"
    ),
    structure(
      data.table(
        dummy_2 = factor(paste0("Y", 1:8)),
        distinct_ids = 5L
      ),
      class = c("sdc_distinct_ids", "data.table", "data.frame"),
      sorted = "dummy_2"
    ),
    structure(
      data.table(
        dummy_3 = c("FR", "BE", "DE", "ES"),
        distinct_ids = c(4L, rep(10L, 3L))
      ),
      class = c("sdc_distinct_ids", "data.table", "data.frame")
      # not sorted, because ?
    )
  ),
  names = paste0("dummy_", 1:3)
)
inter_dt <- CJ(
  dummy_1 = c("M1", "M2"),
  dummy_2 = paste0("Y", 1:8),
  dummy_3 = c("BE", "DE", "FR", "ES")
)
inter_dt <- merge(
  inter_dt,
  model_test_dt[, .(distinct_ids = uniqueN(id)), keyby = .(dummy_1, dummy_2, dummy_3)]
)
inter_dt[, `dummy_1:dummy_2:dummy_3` := paste(dummy_1, dummy_2, dummy_3, sep = ":")]
inter_dt[, paste0("dummy_", 1:3) := NULL]
setcolorder(inter_dt, "dummy_1:dummy_2:dummy_3")
setorder(inter_dt, distinct_ids)
interactions_ref_6 <- structure(
  list(
    structure(
      inter_dt,
      class = c("sdc_distinct_ids", "data.table", "data.frame")
    )
  ),
  names = "dummy_1:dummy_2:dummy_3"
)

# create ref. list
res_6 <- structure(
  list(
    message_options = message_options(),
    message_arguments = message_arguments(id_var = "id"),
    distinct_ids = distinct_ref_6,
    dummies = dummy_ref_6,
    interactions = interactions_ref_6
  ),
  class = c("sdc_model", "list")
)

# test that sdc_model works correctly
# check with equivalent (otherwise attributes for dummys would have to be set)
test_that("sdc_model() returns/works correctly", {
  expect_warning(
    expect_warning(
      capture_output(
        expect_identical(
          sdc_model(model_test_dt, model_6, "id"),
          res_6
        )
      ),
      "Potential disclosure problem: Not enough distinct entities.",
      fixed = TRUE
    ),
    "Potential disclosure problem: Not enough distinct entities."
  )
})


# test arguments in sdc_model ----

# test that sdc_model returns appropriate error
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
