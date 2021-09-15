library(data.table)
data("sdc_model_DT")

# create ref_1 and model_1 for reuse
ref_1 <- structure(
  list(
    options = list_options(),
    settings = list_arguments(id_var = "id"),
    distinct_ids = structure(
      data.table(distinct_ids = 10L),
      class = c("sdc_distinct_ids", "data.table", "data.frame")
    ),
    terms = list(
      x_1 = structure(
        data.table(x_1 = "<non-zero>", distinct_ids = 10L, key = "x_1"),
        class = c("sdc_distinct_ids", "data.table", "data.frame")
      ),
      x_2 = structure(
        data.table(x_2 = "<non-zero>", distinct_ids = 10L, key = "x_2"),
        class = c("sdc_distinct_ids", "data.table", "data.frame")
      )
    )
  ),
  class = c("sdc_model", "list")
)

model_1 <- lm(y ~ x_1 + x_2, data = sdc_model_DT)
summary(model_1)

# no problems ----
test_that("no problems are handles correctly", {
  data("sdc_model_DT")


  expect_equal(
    sdc_model(
      as.data.frame(sdc_model_DT, stringsAsFactors = FALSE), model_1, "id"
    ),
    ref_1,
    ignore_attr = TRUE
  )
})


# too few distinct id's ----
test_that("too few distinct id's are handled correctly", {
  data("sdc_model_DT")

  model_2 <- lm(y ~ x_1 + x_2 + x_3, data = sdc_model_DT)
  summary(model_2)

  # create ref
  dummy_ref_2 <- structure(list(), names = character())
  interactions_ref_2 <- structure(list(), names = character())

  # create ref. list
  ref_2 <- structure(
    list(
      options = list_options(),
      settings = list_arguments(id_var = "id"),
      distinct_ids = structure(
        data.table(distinct_ids = 4L),
        class = c("sdc_distinct_ids", "data.table", "data.frame")
      ),
      terms = list(
        x_1 = structure(
          data.table(x_1 = "<non-zero>", distinct_ids = 4L, key = "x_1"),
          class = c("sdc_distinct_ids", "data.table", "data.frame")
        ),
        x_2 = structure(
          data.table(x_2 = "<non-zero>", distinct_ids = 4L, key = "x_2"),
          class = c("sdc_distinct_ids", "data.table", "data.frame")
        ),
        x_3 = structure(
          data.table(x_3 = "<non-zero>", distinct_ids = 4L, key = "x_3"),
          class = c("sdc_distinct_ids", "data.table", "data.frame")
        )
      )
    ),
    class = c("sdc_model", "list")
  )


  expect_warning(
    expect_equal(
      sdc_model(sdc_model_DT, model_2, "id"),
      ref_2,
      ignore_attr = TRUE
    ),
    paste0(crayon::bold("DISCLOSURE PROBLEM: "),
           "Not enough distinct entities."
    ),
    fixed = TRUE
  )
})


# no problems, with dummys ----
test_that("dummies are handled correctly", {
  data("sdc_model_DT")

  model_3 <- lm(y ~ x_1 + x_2 + dummy_1 + dummy_2, data = sdc_model_DT)
  summary(model_3)

  # create ref
  ref_3 <- structure(
    list(
      options = list_options(),
      settings = list_arguments(id_var = "id"),
      distinct_ids = structure(
        data.table(distinct_ids = 10L),
        class = c("sdc_distinct_ids", "data.table", "data.frame")
      ),
      terms = list(
        x_1 = structure(
          data.table(x_1 = "<non-zero>", distinct_ids = 10L, key = "x_1"),
          class = c("sdc_distinct_ids", "data.table", "data.frame")
        ),
        x_2 = structure(
          data.table(x_2 = "<non-zero>", distinct_ids = 10L, key = "x_2"),
          class = c("sdc_distinct_ids", "data.table", "data.frame")
        ),
        dummy_1 = structure(
          data.table( dummy_1 = factor(c("M1", "M2")), distinct_ids = 10L),
          class = c("sdc_distinct_ids", "data.table", "data.frame"),
          sorted = "dummy_1"
        ),
        dummy_2 = structure(
          data.table(dummy_2 = factor(paste0("Y", 1:8)), distinct_ids = 5L),
          class = c("sdc_distinct_ids", "data.table", "data.frame"),
          sorted = "dummy_2"
        )
      )
    ),
    class = c("sdc_model", "list")
  )

  expect_equal(
    sdc_model(sdc_model_DT, model_3, "id"),
    ref_3,
    ignore_attr = TRUE
  )
})


# only problems with dummy ----
test_that("dummy problems are handled correctly", {
  data("sdc_model_DT")
  model_4 <- lm(y ~ x_1 + x_2 + dummy_3, data = sdc_model_DT)
  summary(model_4)

  # create ref
  ref_4 <- structure(
    list(
      options = list_options(),
      settings = list_arguments(id_var = "id"),
      distinct_ids = structure(
        data.table(distinct_ids = 10L),
        class = c("sdc_distinct_ids", "data.table", "data.frame")
      ),
      terms = list(
        x_1 = structure(
          data.table(x_1 = "<non-zero>", distinct_ids = 10L, key = "x_1"),
          class = c("sdc_distinct_ids", "data.table", "data.frame")
        ),
        x_2 = structure(
          data.table(x_2 = "<non-zero>", distinct_ids = 10L, key = "x_2"),
          class = c("sdc_distinct_ids", "data.table", "data.frame")
        ),
        dummy_3 = structure(
          data.table(
            dummy_3 = factor(c("FR", "BE", "DE", "ES")),
            distinct_ids = c(4L, rep(10L, 3L))
          ),
          class = c("sdc_distinct_ids", "data.table", "data.frame")
        )
      )
    ),
    class = c("sdc_model", "list")
  )


  expect_warning(
    expect_identical(
      sdc_model(sdc_model_DT, model_4, "id"),
      ref_4,
      ignore_attr = TRUE
    ),
    paste0(
      crayon::bold("DISCLOSURE PROBLEM: "),
      "Not enough distinct entities."
    ),
    fixed = TRUE
  )
})


# no problems, with interaction ----
test_that("interactions are handled correctly", {
  data("sdc_model_DT")
  model_5 <- lm(y ~ dummy_1 * dummy_2, data = sdc_model_DT)
  summary(model_5)

  # create ref
  ref_5 <- structure(
    list(
      options = list_options(),
      settings = list_arguments(id_var = "id"),
      distinct_ids = structure(
        data.table(distinct_ids = 10L),
        class = c("sdc_distinct_ids", "data.table", "data.frame")
      ),
      terms = list(
        dummy_1 = structure(
          data.table(dummy_1 = factor(c("S1", "S2")), distinct_ids = 10L),
          class = c("sdc_distinct_ids", "data.table", "data.frame"),
          sorted = "dummy_1"
        ),
        dummy_2 = structure(
          data.table(dummy_2 = factor(paste0("Y", 1:8)), distinct_ids = 5L),
          class = c("sdc_distinct_ids", "data.table", "data.frame"),
          sorted = "dummy_2"
        ),
        `dummy_1:dummy_2` = structure({
          DT <- CJ(c("S1", "S2"), paste0("Y", 1:8))
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

  expect_equal(
    sdc_model(sdc_model_DT, model_5, "id"),
    ref_5,
    ignore_attr = TRUE
  )
})


# interaction with problems ----
test_that("interaction with problems is handled correctly", {
  data("sdc_model_DT")
  model_6 <- lm(y ~ dummy_1 : dummy_2 : dummy_3, data = sdc_model_DT)
  summary(model_6)

  # create ref
  ref_6 <- structure(
    list(
      options = list_options(),
      settings = list_arguments(id_var = "id"),
      distinct_ids = structure(
        data.table(distinct_ids = 10L),
        class = c("sdc_distinct_ids", "data.table", "data.frame")
      ),
      terms = list(
        `dummy_1:dummy_2:dummy_3` = structure(
          structure({
            DT <- CJ(
              dummy_1 = c("S1", "S2"),
              dummy_2 = paste0("Y", 1:8),
              dummy_3 = c("BE", "DE", "FR", "ES")
            )
            DT <- merge(
              DT,
              sdc_model_DT[, .(distinct_ids = uniqueN(id)), keyby = .(dummy_1, dummy_2, dummy_3)]
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


  expect_warning(
    expect_equal(
      sdc_model(sdc_model_DT, model_6, "id"),
      ref_6,
      ignore_attr = TRUE
    ),
    paste(
      crayon::bold("DISCLOSURE PROBLEM:"),
      "Not enough distinct entities."
    ),
    fixed = TRUE
  )
})


# errors ----
test_that("sdc_model() returns appropriate errors", {

  data("sdc_model_DT")

  # error für nichtexistierende Elemente
  warnings <- capture_warnings(
    expect_error(
      sdc_model(sdc_model_DT, wrong_model, "id"),
      "object 'wrong_model' not found",
      fixed = TRUE
    )
  )
  expect_error(
    sdc_model(sdc_model_DT, model_1, "wrong_id"),
    "Assertion on 'id_var' failed: Must be a subset of {'id','y','x_1','x_2','x_3','x_4','dummy_1','dummy_2','dummy_3'}, but is {'wrong_id'}.",
    fixed = TRUE
  )
  expect_error(
    sdc_model(wrong_model_dt, model_1, "id"),
    "object 'wrong_model_dt' not found",
    fixed = TRUE
  )

  # error für id_var unquoted
  expect_error(sdc_model(sdc_model_DT, model_1, id), "object 'id' not found")

  # error für data quoted
  expect_error(
    sdc_model("model_test_dt", model_1, "id"),
    "Assertion on 'data' failed: Must be of type 'data.frame', not 'character'.",
    fixed = TRUE
  )

  # error für missing arguments
  expect_error(
    sdc_model(sdc_model_DT, model_1),
    "Assertion on 'id_var' failed: Must be of type 'string', not 'NULL'.",
    fixed = TRUE
  )
  expect_error(
    sdc_model(sdc_model_DT, id_var = "id"),
    'argument "model" is missing, with no default',
    fixed = TRUE
  )
  expect_error(
    sdc_model(model = model_1, id_var = "id"),
    'argument "data" is missing, with no default',
    fixed = TRUE
  )
  expect_error(
    sdc_model(model = model_1, data = sdc_model_DT[1:10], id_var = "id"),
    "'data' is not the data.frame which was used in 'model'.",
    fixed = TRUE
  )
})


# support for felm ----
if (requireNamespace("lfe", quietly = TRUE)) {
  # simple case (lm)
  test_that("sdc_model() returns/works correctly for simple felm", {

    data("sdc_model_DT")
    options(sdc.id_var = "id")

    felm_1 <- lfe::felm(y ~ x_1 + x_2 | 0 | 0 | 0, data = sdc_model_DT)
    expect_equal(
      sdc_model(sdc_model_DT, felm_1),
      ref_1,
      ignore_attr = TRUE
    )
  })

  # case where id_var is used for clustering
  test_that("sdc_model() returns/works correctly for clustered felm", {


    felm_2 <- lfe::felm(y ~ x_1 + x_2 | id | 0 | id, data = sdc_model_DT)
    expect_equal(
      sdc_model(sdc_model_DT, felm_2, "id"),
      ref_1,
      ignore_attr = TRUE
    )
  })
}

test_that("Bug from #79 is solved", {
  # x_id identifies an id
  data("sdc_model_DT")
  sdc_model_DT[id == "A", x_id := rnorm(.N)]
  sdc_model_DT[id != "A", x_id := 0L]
  model_all_but_one_zero <- lm(y ~ x_id, data = sdc_model_DT)

  expect_warning(
    checkmate::expect_list(
      sdc_model(
        data = sdc_model_DT,
        model = model_all_but_one_zero,
        id_var = "id"
      )
    ),
    paste0(crayon::bold("DISCLOSURE PROBLEM: "),
           "Not enough distinct entities."
    ),
    fixed = TRUE
  )

  # x_id does not identify an id
  sdc_model_DT[id != "A", x_id := rnorm(.N)]
  sdc_model_DT[id == "A", x_id := 0L]
  ref_issue_79 <- structure(
    list(
      options = list_options(),
      settings = list_arguments(id_var = "id"),
      distinct_ids = structure(
        data.table(distinct_ids = 10L),
        class = c("sdc_distinct_ids", "data.table", "data.frame")
      ),
      terms = list(
        x_id = structure(
          data.table(
            x_id = c("<zero>", "<non-zero>"),
            distinct_ids = c(1, 9L)
          ),
          class = c("sdc_distinct_ids", "data.table", "data.frame")
        )
      )
    ),
    class = c("sdc_model", "list")
  )

  model_all_but_one_non_zero <- lm(y ~ x_id, data = sdc_model_DT)

  expect_equal(
    sdc_model(
      data = sdc_model_DT,
      model = model_all_but_one_non_zero,
      id_var = "id"
    ),
    ref_issue_79,
    ignore_attr = TRUE
  )
})
