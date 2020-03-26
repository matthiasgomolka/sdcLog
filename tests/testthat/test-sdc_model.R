library(testthat)
library(data.table)


# create dt for model tests
set.seed(1)
n <- 80

y <- rnorm(n, mean = 120, sd = 8)
model_test_dt <- data.table(
    id = rep_len(LETTERS[1L:10L], n),
    y = y,
    x_1 = jitter(y, factor = 10000),
    x_2 = jitter(y, factor = 15000),
    x_3 = jitter(y, factor = 50000),
    x_4 = jitter(y, factor = 60000),
    dummy_1 = sort(rep_len(paste0("M", 1L:2L), n)),
    dummy_2 = as.factor(rep_len(paste0("Y", 1L:8L), n)),
    dummy_3 = c(rep(rawToChar(as.raw(c(66, 69))), n/4), rep(rawToChar(as.raw(68:69)), n/4), rep(rawToChar(as.raw(c(69, 83))), 36), rep(rawToChar(as.raw(c(70, 82))), 4)),
    key = "id"
)


# create problems id's for x_3
model_test_dt[id %chin% c("A", "B", "C", "D", "E", "F"), x_3 := NA_real_]

# create problems dominance for x_4
model_test_dt[id %chin% c("A", "B"), x_4 := x_4 * 100]


# characteristics variables:
    # id: id variable
    # y: dependent variable
    # independent variables:
    # x_1 & x_2: should lead to no problems at all in model
    # x_3: leads in model to problems with distinct id's
    # x_4: leads in model to problems with dominance
    # dummy_1 & dummy_2: lead in model to no problems at all
    # dummy_3: leads in model to probelms with dummy variable


## set up: models

# model 1:
# all good, no problems at all
# y = β0 + β1*x_1 + β2*x_2 + u
model_1 <- lm(y ~ x_1 + x_2, data = model_test_dt)
summary(model_1)

sdc_model(model_test_dt, model_1, "id")

# model 2:
# problem distinct id's, no dummys
# y = β0 + β1*x_1 + β2*x_2 + β3*x_3 + u
model_2 <- lm(y ~ x_1 + x_2 + x_3, data = model_test_dt)
summary(model_2)

sdc_model(model_test_dt, model_2, "id")

# model 3:
# problem dominance, no dummys
# y = β0 + β1*x_1 + β2*x_2 + β3*x_4 + u
model_3 <- lm(y ~ x_1 + x_2 + x_4, data = model_test_dt)
summary(model_3)

sdc_model(model_test_dt, model_3, "id")


# model 4:
# all good, with dummys
# y = β0 + β1*x_1 + β2*x_2 + β3*dummy_1 + β4*dummy_2 + u
model_4 <- lm(y ~ x_1 + x_2 + dummy_1 + dummy_2, data = model_test_dt)
summary(model_4)

sdc_model(model_test_dt, model_4, "id")


# model 5:
# only problems with dummy (dummy_3)
# y = β0 + β1*x_1 + β2*x_2 + β3*dummy_3 + u
# dummy var trap, so exclude one dummy in dummy_3 col
model_5 <- lm(y ~ x_1 + x_2 + dummy_3, data = model_test_dt)
summary(model_5)

sdc_model(model_test_dt, model_5, "id")

# tests: return, functionality etc.

# test sdc_model ----
context("sdc_model")


# test that sdc_model returns a list
test_that("sdc_model() returns a list", {
    expect_true(is.list(sdc_model(model_test_dt, model_1, "id")))
        capture_output(
    expect_true(is.list(sdc_model(model_test_dt, model_2, "id")))
        )
        capture_output(
    expect_true(is.list(sdc_model(model_test_dt, model_3, "id")))
        )
    expect_true(is.list(sdc_model(model_test_dt, model_4, "id")))
        capture_output(
    expect_true(is.list(sdc_model(model_test_dt, model_5, "id")))
        )
})

# functionality tests

# test that sdc_model() returns warnings, if necessary
# warnings expected:
    # model 2 (distinct id's problem),
    # model 3 (dominance problem)
    # model 5 (dummy problem)

test_that("sdc_model() returns warning, if necessary", {
    expect_warning(sdc_model(model_test_dt, model_2, "id"))
    expect_warning(sdc_model(model_test_dt, model_3, "id"))
    expect_warning(sdc_model(model_test_dt, model_5, "id"))
})

test_that("sdc_model() returns warning, if necessary", {
    capture.output(expect_warning(sdc_model(model_test_dt, model_2, "id")))
    capture.output(expect_warning(sdc_model(model_test_dt, model_3, "id")))
    capture.output(expect_warning(sdc_model(model_test_dt, model_5, "id")))
})



# sdc_model() returns correct messages

# set up:
# model_1, for sdc.info_level = 0|1
options(sdc.info_level = 0)
getOption("sdc.info_level")

model_expect_1_info_0 <- function(x) {
    messages <- capture_messages(x)
    expect_match(
        paste0(messages, collapse = ""),
        paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85 ]\n",
               "[ SETTINGS: id_var: id ]\n",
               collapse = ""),
        fixed = TRUE
    )
}


# test that sdc_model returns correct messages
test_that("sdc_model() returns correct messages", {
    model_expect_1_info_0(
        sdc_model(model_test_dt, model_1, "id"))
})


# model_1, for sdc.info_level = 2
options(sdc.info_level = 2)
getOption("sdc.info_level")

model_expect_1_info_2 <- function(x) {
    messages <- capture_messages(x)
    expect_match(
        paste0(messages, collapse = ""),
        paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85 ]\n",
               "[ SETTINGS: id_var: id ]\n",
               "No problem with number of distinct entities.\n",
               # "no problems with dominance" message?
               #"No problem with dominance.\n",
               "No dummy variables in data.\n",
               collapse = ""),
        fixed = TRUE
    )
}


test_that("sdc_model() returns correct messages", {
    model_expect_1_info_2(
        sdc_model(model_test_dt, model_1, "id"))
})



# model_2, for sdc.info_level = 0|1
options(sdc.info_level = 0)
getOption("sdc.info_level")

model_expect_2_info_0 <- function(x) {
    messages <- capture_messages(x)
    expect_match(
        paste0(messages, collapse = ""),
        paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85 ]\n",
               "[ SETTINGS: id_var: id ]\n",
               collapse = ""),
        fixed = TRUE
    )
}


# test that sdc_model returns correct messages
test_that("sdc_model() returns correct messages", {
    model_expect_2_info_0(
        capture_output(sdc_model(model_test_dt, model_2, "id")))
})


# model_2, for sdc.info_level = 2
options(sdc.info_level = 2)
getOption("sdc.info_level")

model_expect_2_info_2 <- function(x) {
    messages <- capture_messages(x)
    expect_match(
        paste0(messages, collapse = ""),
        paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85 ]\n",
               "[ SETTINGS: id_var: id ]\n",
               # "no problems with dominance" message?
               #"No problem with dominance.\n",
               "No dummy variables in data",
               collapse = ""),
        fixed = TRUE
    )
}


# test that sdc_model returns correct messages
test_that("sdc_model() returns correct messages", {
    model_expect_2_info_2(
        capture_output(sdc_model(model_test_dt, model_2, "id")))
})


# model_3, for sdc.info_level = 0|1
options(sdc.info_level = 0)
getOption("sdc.info_level")

model_expect_3_info_0 <- function(x) {
    messages <- capture_messages(x)
    expect_match(
        paste0(messages, collapse = ""),
        paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85 ]\n",
               "[ SETTINGS: id_var: id ]\n",
               collapse = ""),
        fixed = TRUE
    )
}


# test that sdc_model returns correct messages
test_that("sdc_model() returns correct messages", {
    model_expect_3_info_0(
        capture_output(sdc_model(model_test_dt, model_3, "id")))
})


# model_3, for sdc.info_level = 2
options(sdc.info_level = 2)
getOption("sdc.info_level")

model_expect_3_info_2 <- function(x) {
    messages <- capture_messages(x)
    expect_match(
        paste0(messages, collapse = ""),
        paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85 ]\n",
               "[ SETTINGS: id_var: id ]\n",
               "No problem with number of distinct entities.\n",
               "No dummy variables in data",
               collapse = ""),
        fixed = TRUE
    )
}


# test that sdc_model returns correct messages
test_that("sdc_model() returns correct messages", {
    model_expect_3_info_2(
        capture_output(sdc_model(model_test_dt, model_3, "id")))
})


# model_4, for sdc.info_level = 0|1
options(sdc.info_level = 0)
getOption("sdc.info_level")

model_expect_4_info_0 <- function(x) {
    messages <- capture_messages(x)
    expect_match(
        paste0(messages, collapse = ""),
        paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85 ]\n",
               "[ SETTINGS: id_var: id ]\n",
               collapse = ""),
        fixed = TRUE
    )
}


# test that sdc_model returns correct messages
test_that("sdc_model() returns correct messages", {
    model_expect_4_info_0(
        capture_output(sdc_model(model_test_dt, model_4, "id")))
})


# model_4, for sdc.info_level = 2
options(sdc.info_level = 2)
getOption("sdc.info_level")

model_expect_4_info_2 <- function(x) {
    messages <- capture_messages(x)
    expect_match(
        paste0(messages, collapse = ""),
        paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85 ]\n",
               "[ SETTINGS: id_var: id ]\n",
               "No problem with number of distinct entities.\n",
               # "no problems with dominance" message?
               #"No problem with dominance.\n",
               # problems here
               #"Output complies to RDSC rules.\n",
               collapse = ""),
        fixed = TRUE
    )
}


# test that sdc_model returns correct messages
test_that("sdc_model() returns correct messages", {
    model_expect_4_info_2(
        capture_output(sdc_model(model_test_dt, model_4, "id")))
})




############# tests to get all together
model_ref_1 <- data.table(distinct_ids = numeric())
class(model_ref_1)    <- c("sdc_counts", class(model_ref_1))

expect_message(sdc_model(model_test_dt, model_1, "id"),
               "[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85 ]\n",
               "[ SETTINGS: id_var: id ]\n",
               model_ref_1,
               "No dummy variables in data.")

expect_message(sdc_model(model_test_dt, model_1, "id"),
               opt,
               model_ref_1,
               "No dummy variables in data.")

opt <- paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85 ]\n",
       "[ SETTINGS: id_var: id ]\n",
       collapse = "")

opt

model_ref_2 <- data.table(distinct_ids = 4L)
class(model_ref_2) <- c("sdc_counts", class(model_ref_2))

expect_output(sdc_model(model_test_dt, model_2, "id"),
               "[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85 ]\n",
               "[ SETTINGS: id_var: id ]\n",
               model_ref_2,
               "No dummy variables in data.")


model_expect <- function(x) {
    messages <- capture_messages(x)
    output <- capture_output(x)
    expect_match(
        paste0(output, collapse = ""),
        paste0(model_ref_2,
               collapse = ""),
        fixed = TRUE
    )
    expect_match(
        paste0(messages, collapse = ""),
        paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85 ]\n",
               "[ SETTINGS: id_var: id ]\n",
               collapse = ""),
        fixed = TRUE
    )
}

model_expect <- function(x) {
    messages <- eval(substitute(capture.output(x, type = "message")))
    output <- eval(substitute(capture.output(x, type = "output")))
    warning <- eval(substitute(capture_warning(x)))
    expect_match(
        paste0(output, collapse = ""),
        paste0(model_ref_2,
               collapse = ""),
        fixed = TRUE
    )
    expect_match(
        messages,
        paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85 ]\n",
               "[ SETTINGS: id_var: id ]\n",
               "No dummy variables in data.",
               warning,
               collapse = ""),
        fixed = TRUE
    )
}


test_that("sdc_model() returns correct messages", {
    model_expect(
        sdc_model(model_test_dt, model_2, "id"))
})

model_expect <- function(x) {
    expect_match(x,
        model_ref_2,
        fixed = TRUE)
    }


    expect_match(
        paste0(output, collapse = ""),
        paste0(model_ref_2,
               collapse = ""),
        fixed = TRUE
    )
    expect_match(
        paste0(messages, collapse = ""),
        paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85 ]\n",
               "[ SETTINGS: id_var: id ]\n",
               collapse = ""),
        fixed = TRUE
    )
}

###################

# first set up output
# model_2
model_ref_2 <- data.table(distinct_ids = 4L)
class(model_ref_2)    <- c("sdc_counts", class(model_ref_2))

model_expect_2_info_0 <- function(x) {
    output <- capture_output(x)
    expect_match(
        paste0(output, collapse = ""),
        paste0(model_ref_2,
               collapse = ""),
        fixed = TRUE
    )
}

test_that("sdc_model() returns correct output", {
    model_expect_2_info_0(
        sdc_model(model_test_dt, model_2, "id"))
})


model_expect_2 <- function(x) {
    expect_match(x,
        "[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85 ]\n",
        "[ SETTINGS: id_var: id ]\n",
        model_ref_2,
        fixed = TRUE
    )
}

model_expect_2_info_0 <- function(x) {
    output <- capture_output(x)
    expect_match(
        paste0(output, collapse = ""),
        paste0(model_ref_2,
               collapse = ""),
        fixed = TRUE
    )
}


test_that("sdc_model() returns correct", {
    model_expect_2(
        sdc_model(model_test_dt, model_2, "id"))
})




# test für model not supported
# model finden, welches nicht supported wird
# tests für versch. Modeltypen


