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
ref_1 <- structure(
  list(
    message_options = message_options(),
    message_arguments = message_arguments(id_var = "id"),
    distinct_ids = structure(
      data.table(distinct_ids = 10L),
      class = c("sdc_distinct_ids", "data.table", "data.frame")
    ),
    terms = list(
      x_1 = structure(
        data.table(
          x_1 = "<non-zero>",
          distinct_ids = 10L,
          key = "x_1"
        ),
        class = c("sdc_distinct_ids", "data.table", "data.frame")
      ),
      x_2 = structure(
        data.table(
          x_2 = "<non-zero>",
          distinct_ids = 10L,
          key = "x_2"
        ),
        class = c("sdc_distinct_ids", "data.table", "data.frame")
      )
    )
  ),
  class = c("sdc_model", "list")
)


# test that sdc_model works correctly
test_that("sdc_model() returns/works correctly", {
  expect_equal(sdc_model(model_test_dt, model_1, "id"), ref_1)
})

# model 2 ----
# problem distinct id's, no dummys
# y = β0 + β1*x_1 + β2*x_2 + β3*x_3 + u
model_2 <- lm(y ~ x_1 + x_2 + x_3, data = model_test_dt)
summary(model_2)

# create ref
dummy_ref_2 <- structure(list(), names = character())
interactions_ref_2 <- structure(list(), names = character())

# create ref. list
ref_2 <- structure(
  list(
    message_options = message_options(),
    message_arguments = message_arguments(id_var = "id"),
    distinct_ids = structure(
      data.table(distinct_ids = 4L),
      class = c("sdc_distinct_ids", "data.table", "data.frame")
    ),
    terms = list(
      x_1 = structure(
        data.table(
          x_1 = "<non-zero>",
          distinct_ids = 4L,
          key = "x_1"
        ),
        class = c("sdc_distinct_ids", "data.table", "data.frame")
      ),
      x_2 = structure(
        data.table(
          x_2 = "<non-zero>",
          distinct_ids = 4L,
          key = "x_2"
        ),
        class = c("sdc_distinct_ids", "data.table", "data.frame")
      ),
      x_3 = structure(
        data.table(
          x_3 = "<non-zero>",
          distinct_ids = 4L,
          key = "x_3"
        ),
        class = c("sdc_distinct_ids", "data.table", "data.frame")
      )
    )
  ),
  class = c("sdc_model", "list")
)

# test that sdc_model works correctly
test_that("sdc_model() returns/works correctly", {
  expect_warning(
    expect_equal(
      sdc_model(model_test_dt, model_2, "id"),
      ref_2
    ),
    paste0(
      crayon::bold("POTENTIAL DISCLOSURE PROBLEM: "),
      "Not enough distinct entities."
    ),
    fixed = TRUE
  )
})


# model 3 ----
# all good, with dummys
# y = β0 + β1*x_1 + β2*x_2 + β3*dummy_1 + β4*dummy_2 + u
model_3 <- lm(y ~ x_1 + x_2 + dummy_1 + dummy_2, data = model_test_dt)
summary(model_3)

# create ref
ref_3 <- structure(
  list(
    message_options = message_options(),
    message_arguments = message_arguments(id_var = "id"),
    distinct_ids = structure(
      data.table(distinct_ids = 10L),
      class = c("sdc_distinct_ids", "data.table", "data.frame")
    ),
    terms = list(
      x_1 = structure(
        data.table(
          x_1 = "<non-zero>",
          distinct_ids = 10L,
          key = "x_1"
        ),
        class = c("sdc_distinct_ids", "data.table", "data.frame")
      ),
      x_2 = structure(
        data.table(
          x_2 = "<non-zero>",
          distinct_ids = 10L,
          key = "x_2"
        ),
        class = c("sdc_distinct_ids", "data.table", "data.frame")
      ),
      dummy_1 = structure(
        data.table(
          dummy_1 = c("M1", "M2"),
          distinct_ids = 10L
        ),
        class = c("sdc_distinct_ids", "data.table", "data.frame"),
        sorted = "dummy_1"
      ),
      dummy_2 = structure(
        data.table(
          dummy_2 = factor(paste0("Y", 1:8)),
          distinct_ids = 5L
        ),
        class = c("sdc_distinct_ids", "data.table", "data.frame"),
        sorted = "dummy_2"
      )
    )
  ),
  class = c("sdc_model", "list")
)

# test that sdc_model works correctly
# check with equivalent (otherwise attributes (key) for dummys would have to be
# set)
test_that("sdc_model() returns/works correctly", {
  expect_identical(
    sdc_model(model_test_dt, model_3, "id"),
    ref_3
  )
})


# model 4 ----
# only problems with dummy (dummy_3)
# y = β0 + β1*x_1 + β2*x_2 + β3*dummy_3 + u
model_4 <- lm(y ~ x_1 + x_2 + dummy_3, data = model_test_dt)
summary(model_4)

# create ref
ref_4 <- structure(
  list(
    message_options = message_options(),
    message_arguments = message_arguments(id_var = "id"),
    distinct_ids = structure(
      data.table(distinct_ids = 10L),
      class = c("sdc_distinct_ids", "data.table", "data.frame")
    ),
    terms = list(
      x_1 = structure(
        data.table(
          x_1 = "<non-zero>",
          distinct_ids = 10L,
          key = "x_1"
        ),
        class = c("sdc_distinct_ids", "data.table", "data.frame")
      ),
      x_2 = structure(
        data.table(
          x_2 = "<non-zero>",
          distinct_ids = 10L,
          key = "x_2"
        ),
        class = c("sdc_distinct_ids", "data.table", "data.frame")
      ),
      dummy_3 = structure(
        data.table(
          dummy_3 = c("FR", "BE", "DE", "ES"),
          distinct_ids = c(4L, rep(10L, 3L))
        ),
        class = c("sdc_distinct_ids", "data.table", "data.frame")
      )
    )
  ),
  class = c("sdc_model", "list")
)

# test that sdc_model works correctly
# check with equivalent (otherwise attributes for dummys would have to be set)
test_that("sdc_model() returns/works correctly", {
  expect_warning(
    expect_identical(
      sdc_model(model_test_dt, model_4, "id"),
      ref_4
    ),
    paste0(
      crayon::bold("POTENTIAL DISCLOSURE PROBLEM: "),
      "Not enough distinct entities."
    ),
    fixed = TRUE
  )
})


# model 5 ----
# interaction without problems
# y = β0 + β1*dummy_1 + β2*dummy_2 + β3*dummy_1*dummy_2 + u
model_5 <- lm(y ~ dummy_1 * dummy_2, data = model_test_dt)
summary(model_5)

# create ref
ref_5 <- structure(
  list(
    message_options = message_options(),
    message_arguments = message_arguments(id_var = "id"),
    distinct_ids = structure(
      data.table(distinct_ids = 10L),
      class = c("sdc_distinct_ids", "data.table", "data.frame")
    ),
    terms = list(
      dummy_1 = structure(
        data.table(
          dummy_1 = c("M1", "M2"),
          distinct_ids = 10L
        ),
        class = c("sdc_distinct_ids", "data.table", "data.frame"),
        sorted = "dummy_1"
      ),
      dummy_2 = structure(
        data.table(
          dummy_2 = factor(paste0("Y", 1:8)),
          distinct_ids = 5L
        ),
        class = c("sdc_distinct_ids", "data.table", "data.frame"),
        sorted = "dummy_2"
      ),
      `dummy_1:dummy_2` = structure({
        DT <- CJ(c("M1", "M2"), paste0("Y", 1:8))
        DT <- DT[, list(
          `dummy_1:dummy_2` = paste(V1, V2, sep = ":"),
          distinct_ids = 5L
        )]
        setkeyv(DT, "dummy_1:dummy_2")
      },
      class = c("sdc_distinct_ids", "data.table", "data.frame")
      )
    )
  ),
  class = c("sdc_model", "list")
)


# test that sdc_model works correctly
# check with equivalent (otherwise attributes for dummies would have to be set)
test_that("sdc_model() returns/works correctly", {
  expect_identical(
    sdc_model(model_test_dt, model_5, "id"),
    ref_5
  )
})

# model 6 ----
# interaction with problems
# y = β0 + β1*dummy_1 + β2*dummy_2 + β3*dummy_1*dummy_2 + u
model_6 <- lm(y ~ dummy_1 : dummy_2 : dummy_3, data = model_test_dt)
summary(model_6)

# create ref

interactions_ref_6 <- structure(
  list(
    structure(
      DT,
      class = c("sdc_distinct_ids", "data.table", "data.frame")
    )
  ),
  names = "dummy_1:dummy_2:dummy_3"
)

# create ref. list
ref_6 <- structure(
  list(
    message_options = message_options(),
    message_arguments = message_arguments(id_var = "id"),
    distinct_ids = structure(
      data.table(distinct_ids = 10L),
      class = c("sdc_distinct_ids", "data.table", "data.frame")
    ),
    terms = list(
      `dummy_1:dummy_2:dummy_3` = structure(
        structure({
          DT <- CJ(
            dummy_1 = c("M1", "M2"),
            dummy_2 = paste0("Y", 1:8),
            dummy_3 = c("BE", "DE", "FR", "ES")
          )
          DT <- merge(
            DT,
            model_test_dt[, .(distinct_ids = uniqueN(id)), keyby = .(dummy_1, dummy_2, dummy_3)]
          )
          DT <- DT[, list(
            `dummy_1:dummy_2:dummy_3` = paste(
              dummy_1, dummy_2, dummy_3, sep = ":"
            ),
            distinct_ids
          )]
          setorder(DT, distinct_ids)
        },
        class = c("sdc_distinct_ids", "data.table", "data.frame")
        )
      )
    )
  ),
  class = c("sdc_model", "list")
)

# test that sdc_model works correctly
# check with equivalent (otherwise attributes for dummys would have to be set)
test_that("sdc_model() returns/works correctly", {
  expect_warning(
    expect_identical(
      sdc_model(model_test_dt, model_6, "id"),
      ref_6
    ),
    paste(
      crayon::bold("POTENTIAL DISCLOSURE PROBLEM:"),
      "Not enough distinct entities."
    ),
    fixed = TRUE
  )
})


# test arguments in sdc_model ----

# test that sdc_model returns appropriate error
test_that("sdc_model() returns appropriate error", {

  # error für nichtexistierende Elemente
  warnings <- capture_warnings(
    expect_error(
      sdc_model(model_test_dt, wrong_model, "id"),
      "object 'wrong_model' not found",
      fixed = TRUE
    )
  )
  expect_error(
    sdc_model(model_test_dt, model_1, "wrong_id"),
    "Assertion on 'id_var' failed: Must be a subset of {'id','y','x_1','x_2','x_3','dummy_1','dummy_2','dummy_3'}, but is {'wrong_id'}.",
    fixed = TRUE
  )
  expect_error(
    sdc_model(wrong_model_dt, model_1, "id"),
    "object 'wrong_model_dt' not found",
    fixed = TRUE
  )

  # error für id_var unquoted
  expect_error(sdc_model(model_test_dt, model_1, id), "object 'id' not found")

  # error für data quoted
  expect_error(
    sdc_model("model_test_dt", model_1, "id"),
    "Assertion on 'data' failed: Must be of type 'data.frame', not 'character'.",
    fixed = TRUE
  )

  # error für missing arguments
  expect_error(
    sdc_model(model_test_dt, model_1),
    "Assertion on 'id_var' failed: Must be of type 'string', not 'NULL'.",
    fixed = TRUE
  )
  expect_error(
    sdc_model(model_test_dt, id_var = "id"),
    'argument "model" is missing, with no default',
    fixed = TRUE
  )
  expect_error(
    sdc_model(model = model_1, id_var = "id"),
    'argument "data" is missing, with no default',
    fixed = TRUE
  )
  expect_error(
    sdc_model(model = model_1, data = model_test_dt[1:10], id_var = "id"),
    "'data' is not the data.frame which was used in 'model'.",
    fixed = TRUE
  )
})
