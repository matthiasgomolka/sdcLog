library(data.table)


# test print.sdc_distinct_ids ----
distinct_ids_1 <- data.table(distinct_ids = integer(0L))

test_that("print.sdc_distinct_ids works for most simple case", {
  options(sdc.info_level = 0L)
  expect_silent(print.sdc_distinct_ids(distinct_ids_1))

  options(sdc.info_level = 1L)
  expect_silent(print.sdc_distinct_ids(distinct_ids_1))

  options(sdc.info_level = 2L)
  expect_message(
    print.sdc_distinct_ids(distinct_ids_1),
    "No problem with number of distinct entities.",
    fixed = TRUE
  )
})

distinct_ids_2 <- data.table(distinct_ids = 3L)
expect_print.sdc_distinct_ids_2 <- function(x) {
  expect_output(
    expect_identical(x, distinct_ids_2),
    "Not enough distinct entities:\n",
    fixed = TRUE
  )
}

test_that("print.sdc_distinct_ids works for problematic case", {
  options(sdc.info_level = 0L)
  expect_print.sdc_distinct_ids_2(print.sdc_distinct_ids(distinct_ids_2))

  options(sdc.info_level = 1L)
  expect_print.sdc_distinct_ids_2(print.sdc_distinct_ids(distinct_ids_2))

  options(sdc.info_level = 2L)
  expect_print.sdc_distinct_ids_2(print.sdc_distinct_ids(distinct_ids_2))
})

distinct_ids_3 <- data.table(sector = paste0("S", 1:2), distinct_ids = 3L:4L)
expect_print.sdc_distinct_ids_3 <- function(x) {
  expect_output(
    expect_identical(x, distinct_ids_3),
    "Not enough distinct entities:\n",
    fixed = TRUE
  )
}

test_that("print.sdc_distinct_ids works for problematic by case", {
  options(sdc.info_level = 0L)
  expect_print.sdc_distinct_ids_3(print.sdc_distinct_ids(distinct_ids_3))

  options(sdc.info_level = 1L)
  expect_print.sdc_distinct_ids_3(print.sdc_distinct_ids(distinct_ids_3))

  options(sdc.info_level = 2L)
  expect_print.sdc_distinct_ids_3(print.sdc_distinct_ids(distinct_ids_3))
})


# test print.sdc_dominance ----
dominance_1 <- data.table(value_share = numeric(0))

test_that("print.sdc_dominance works for most simple case", {
  options(sdc.info_level = 0L)
  expect_silent(print.sdc_dominance(dominance_1))

  options(sdc.info_level = 1L)
  expect_silent(print.sdc_dominance(dominance_1))

  options(sdc.info_level = 2L)
  expect_message(
    print.sdc_dominance(dominance_1),
    "No problem with dominance.",
    fixed = TRUE
  )
})

dominance_2 <- data.table(value_share = 0.9)
expect_print.sdc_dominance_2 <- function(x) {
  expect_output(
    expect_identical(x, dominance_2),
    "Dominant entities:\n",
    fixed = TRUE
  )
}

test_that("print.sdc_dominance works for problematic case", {
  options(sdc.info_level = 0L)
  expect_print.sdc_dominance_2(print.sdc_dominance(dominance_2))

  options(sdc.info_level = 1L)
  expect_print.sdc_dominance_2(print.sdc_dominance(dominance_2))

  options(sdc.info_level = 2L)
  expect_print.sdc_dominance_2(print.sdc_dominance(dominance_2))
})

dominance_3 <- data.table(sector = paste0("S", 1:2), value_share = c(0.88, 0.9))
expect_print.sdc_dominance_3 <- function(x) {
  expect_output(
    expect_identical(x, dominance_3),
    "Dominant entities:\n",
    fixed = TRUE
  )
}

test_that("print.sdc_dominance works for problematic by case", {
  options(sdc.info_level = 0L)
  expect_print.sdc_dominance_3(print.sdc_dominance(dominance_3))

  options(sdc.info_level = 1L)
  expect_print.sdc_dominance_3(print.sdc_dominance(dominance_3))

  options(sdc.info_level = 2L)
  expect_print.sdc_dominance_3(print.sdc_dominance(dominance_3))
})


# test print.sdc_descriptives ----
descriptives_1 <- list(
  message_options = message_options(),
  message_arguments = message_arguments(id_var = "id", val_var = "val"),
  distinct_ids = distinct_ids_1,
  dominance = dominance_1
)

class(descriptives_1[["distinct_ids"]]) <-
  c("sdc_distinct_ids", class(descriptives_1[["distinct_ids"]]))
class(descriptives_1[["dominance"]]) <-
  c("sdc_dominance", class(descriptives_1[["dominance"]]))
class(descriptives_1) <- c("sdc_descriptives", class(descriptives_1))


test_that("print.sdc_descriptives works for most simple case", {
  options(sdc.info_level = 0L)
  messages <- capture_messages(print.sdc_descriptives(descriptives_1))
  expect_match(
    paste0(messages, collapse = ""),
    paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | ",
      "sdc.share_dominance: 0.85 ]\n",
      "[ SETTINGS: id_var: id | val_var: val ]\n",
      collapse = ""
    ),
    fixed = TRUE
  )

  options(sdc.info_level = 1L)
  messages <- capture_messages(print.sdc_descriptives(descriptives_1))
  expect_match(
    paste0(messages, collapse = ""),
    paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | ",
      "sdc.share_dominance: 0.85 ]\n",
      "[ SETTINGS: id_var: id | val_var: val ]\n",
      "Output complies to RDC rules.",
      collapse = ""
    ),
    fixed = TRUE
  )

  options(sdc.info_level = 2L)
  messages <- capture_messages(print.sdc_descriptives(descriptives_1))
  expect_match(
    paste0(messages, collapse = ""),
    paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | ",
      "sdc.share_dominance: 0.85 ]\n",
      "[ SETTINGS: id_var: id | val_var: val ]\n",
      "No problem with number of distinct entities.\n",
      "No problem with dominance.\n",
      "Output complies to RDC rules.",
      collapse = ""
    ),
    fixed = TRUE
  )
})

descriptives_2 <- list(
  message_options = message_options(),
  message_arguments = message_arguments(id_var = "id", val_var = "val"),
  distinct_ids = distinct_ids_2,
  dominance = dominance_2
)

class(descriptives_2[["distinct_ids"]]) <-
  c("sdc_distinct_ids", class(descriptives_2[["distinct_ids"]]))
class(descriptives_2[["dominance"]]) <-
  c("sdc_dominance", class(descriptives_2[["dominance"]]))
class(descriptives_2) <- c("sdc_descriptives", class(descriptives_2))


expect_print.sdc_descriptives_2 <- function(x) {
  output <- capture_output_lines({
    message <- capture_messages(x)
  })

  expect_match(
    paste0(message, collapse = ""),
    paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | ",
      "sdc.share_dominance: 0.85 ]\n",
      "[ SETTINGS: id_var: id | val_var: val ]\n",
      collapse = ""
    ),
    fixed = TRUE
  )

  expect_match(
    output[1],
    "Not enough distinct entities:",
    fixed = TRUE
  )

  expect_match(
    output[4],
    "Dominant entities:",
    fixed = TRUE
  )
}


test_that("print.sdc_descriptives works for problematic case", {
  options(sdc.info_level = 0L)
  expect_print.sdc_descriptives_2(print.sdc_descriptives(descriptives_2))

  options(sdc.info_level = 1L)
  expect_print.sdc_descriptives_2(print.sdc_descriptives(descriptives_2))

  options(sdc.info_level = 2L)
  expect_print.sdc_descriptives_2(print.sdc_descriptives(descriptives_2))
})


descriptives_3 <- list(
  message_options = message_options(),
  message_arguments = c(
    "[ SETTINGS: ",
    paste0("id_var: ", "id"),
    paste0(" | val_var: ", "val"),
    paste0(" | by: ", "sector"),
    " ]"
  ),
  distinct_ids = distinct_ids_3,
  dominance = dominance_3
)

class(descriptives_3[["distinct_ids"]]) <-
  c("sdc_distinct_ids", class(descriptives_3[["distinct_ids"]]))
class(descriptives_3[["dominance"]]) <-
  c("sdc_dominance", class(descriptives_3[["dominance"]]))
class(descriptives_3) <- c("sdc_descriptives", class(descriptives_3))


expect_print.sdc_descriptives_3 <- function(x) {
  output <- capture_output_lines({
    message <- capture_messages(x)
  })

  expect_match(
    paste0(message, collapse = ""),
    paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | ",
      "sdc.share_dominance: 0.85 ]\n",
      "[ SETTINGS: id_var: id | val_var: val | by: sector ]\n",
      collapse = ""
    ),
    fixed = TRUE
  )

  expect_match(
    output[1],
    "Not enough distinct entities:",
    fixed = TRUE
  )

  expect_match(
    output[5],
    "Dominant entities:",
    fixed = TRUE
  )
}


test_that("print.sdc_descriptives works for problematic by case", {
  options(sdc.info_level = 0L)
  expect_print.sdc_descriptives_3(print.sdc_descriptives(descriptives_3))

  options(sdc.info_level = 1L)
  expect_print.sdc_descriptives_3(print.sdc_descriptives(descriptives_3))

  options(sdc.info_level = 2L)
  expect_print.sdc_descriptives_3(print.sdc_descriptives(descriptives_3))
})


# test print.sdc_model ----
### create model ref.

## simple cases
# distinct_ids
class(distinct_ids_1) <- c("sdc_distinct_ids", class(distinct_ids_1))

# dominance_list
# dominance_list_1 for simple cases
y <- data.table(value_share = numeric(0))
class(y) <- c("sdc_dominance", class(y))

x_1 <- data.table(value_share = numeric(0))
class(x_1) <- c("sdc_dominance", class(x_1))

x_2 <- data.table(value_share = numeric(0))
class(x_2) <- c("sdc_dominance", class(x_2))

dominance_list_1 <- list(y, x_1, x_2)
names(dominance_list_1) <- c("y", "x_1", "x_2")

# dummy_list
# dummy_list_1 for simple cases
dummy_list_1 <- list()
dummy_vars <- as.character()
names(dummy_list_1) <- dummy_vars


model_ref_1 <- list(
  message_options = message_options(),
  message_arguments = message_arguments(id_var = "id"),
  distinct_ids = distinct_ids_1,
  dominance_list = dominance_list_1,
  dummies = dummy_list_1
)

class(model_ref_1) <- c("sdc_model", class(model_ref_1))

test_that("print.sdc_model works for most simple case", {
  options(sdc.info_level = 0L)
  invisible(capture.output({
    messages <- capture_messages(print.sdc_model(model_ref_1))
  }))
  expect_match(
    paste0(messages, collapse = ""),
    paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | ",
      "sdc.share_dominance: 0.85 ]\n",
      "[ SETTINGS: id_var: id ]\n",
      collapse = ""
    ),
    fixed = TRUE
  )

  options(sdc.info_level = 1L)
  invisible(capture.output({
    messages <- capture_messages(print.sdc_model(model_ref_1))
  }))
  expect_match(
    paste0(messages, collapse = ""),
    paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | ",
      "sdc.share_dominance: 0.85 ]\n",
      "[ SETTINGS: id_var: id ]\n",
      "Output complies to RDC rules.\n",
      collapse = ""
    ),
    fixed = TRUE
  )

  options(sdc.info_level = 2L)
  invisible(capture.output({
    messages <- capture_messages(print.sdc_model(model_ref_1))
  }))
  expect_match(
    paste0(messages, collapse = ""),
    paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | ",
      "sdc.share_dominance: 0.85 ]\n",
      "[ SETTINGS: id_var: id ]\n",
      "No problem with number of distinct entities.\n",
      "Output complies to RDC rules.\n",
      collapse = ""
    ),
    fixed = TRUE
  )
})


## problematic cases (every variable leads to problems)
# distinct_ids
class(distinct_ids_2) <- c("sdc_distinct_ids", class(distinct_ids_2))

# dominance_list
# dominance_list_2 for problematic cases
z <- data.table(value_share = 0.9)
class(z) <- c("sdc_dominance", class(z))

x_3 <- data.table(value_share = 0.9)
class(x_3) <- c("sdc_dominance", class(x_3))

dominance_list_2 <- list(z, x_3)
names(dominance_list_2) <- c("z", "x_3")

# dummy_list
# dummy_list_2 for problematic cases
dummy_1 <- data.table(
  dummy_1 = "S",
  distinct_ids = 4
)
class(dummy_1) <- c("sdc_distinct_ids", class(dummy_1))
dummy_list_2 <- list(dummy_1)
dummy_vars_2 <- "dummy_1"
names(dummy_list_2) <- dummy_vars_2


model_ref_2 <- list(
  message_options = message_options(),
  message_arguments = message_arguments(id_var = "id"),
  distinct_ids = distinct_ids_2,
  dummies = dummy_list_2
)

class(model_ref_2) <- c("sdc_model", class(model_ref_2))

expect_print.sdc_model_2 <- function(x) {
  output <- capture_output_lines({
    message <- capture_messages(x)
  })

  expect_match(
    paste0(message, collapse = ""),
    paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | ",
      "sdc.share_dominance: 0.85 ]\n",
      "[ SETTINGS: id_var: id ]\n",
      collapse = ""
    ),
    fixed = TRUE
  )

  expect_match(
    output[1],
    "Not enough distinct entities:",
    fixed = TRUE
  )

  expect_match(
    output[5],
    "Not enough distinct entities:",
    fixed = TRUE
  )
}


test_that("print.sdc_model works for problematic cases", {
  options(sdc.info_level = 0L)
  expect_print.sdc_model_2(print.sdc_model(model_ref_2))

  options(sdc.info_level = 1L)
  expect_print.sdc_model_2(print.sdc_model(model_ref_2))

  options(sdc.info_level = 2L)
  expect_print.sdc_model_2(print.sdc_model(model_ref_2))
})


## complex problematic cases (some variables lead to problems)
# distinct_ids
class(distinct_ids_3) <- c("sdc_distinct_ids", class(distinct_ids_3))

# dominance_list
# dominance_list_3 for problematic cases
z <- data.table(value_share = 0.9)
class(z) <- c("sdc_dominance", class(z))

x_3 <- data.table(value_share = numeric(0))
class(x_3) <- c("sdc_dominance", class(x_3))

dominance_list_3 <- list(z, x_3)
names(dominance_list_3) <- c("z", "x_3")

# dummy_list
# dummy_list_3 for some problematic cases
dummy_1 <- data.table(
  dummy_1 = "S",
  distinct_ids = 4
)
class(dummy_1) <- c("sdc_distinct_ids", class(dummy_1))

dummy_2 <- data.table(distinct_ids = numeric(0))
class(dummy_2) <- c("sdc_distinct_ids", class(dummy_2))

dummy_list_3 <- list(dummy_1, dummy_2)
dummy_vars_3 <- c("dummy_1", "dummy_2")
names(dummy_list_3) <- dummy_vars_3


model_ref_3 <- list(
  message_options = message_options(),
  message_arguments = message_arguments(id_var = "id"),
  distinct_ids = distinct_ids_3,
  dummies = dummy_list_3
)

class(model_ref_3) <- c("sdc_model", class(model_ref_3))


expect_print.sdc_model_3_info_0_1 <- function(x) {
  output <- capture_output_lines({
    message <- capture_messages(x)
  })

  expect_match(
    paste0(message, collapse = ""),
    paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | ",
      "sdc.share_dominance: 0.85 ]\n",
      "[ SETTINGS: id_var: id ]\n",
      collapse = ""
    ),
    fixed = TRUE
  )

  expect_match(
    output[1],
    "Not enough distinct entities:",
    fixed = TRUE
  )

  expect_match(
    output[6],
    "Not enough distinct entities:",
    fixed = TRUE
  )
}

expect_print.sdc_model_3_info_2 <- function(x) {
  output <- capture_output_lines({
    message <- capture_messages(x)
  })

  expect_match(
    paste0(message, collapse = ""),
    paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | ",
      "sdc.share_dominance: 0.85 ]\n",
      "[ SETTINGS: id_var: id ]\n",
      "No problem with number of distinct entities.\n",
      collapse = ""
    ),
    fixed = TRUE
  )

  expect_match(
    output[1],
    "Not enough distinct entities:",
    fixed = TRUE
  )

  expect_match(
    output[6],
    "Not enough distinct entities:",
    fixed = TRUE
  )
}


test_that("print.sdc_model works for problematic cases", {
  options(sdc.info_level = 0L)
  expect_print.sdc_model_3_info_0_1(print.sdc_model(model_ref_3))

  options(sdc.info_level = 1L)
  expect_print.sdc_model_3_info_0_1(print.sdc_model(model_ref_3))

  options(sdc.info_level = 2L)
  expect_print.sdc_model_3_info_2(print.sdc_model(model_ref_3))
})
