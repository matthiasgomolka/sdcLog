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
# on first sight:
# "no problems with dominance" message?

# model 2:
# problem distinct id's, no dummys
# y = β0 + β1*x_1 + β2*x_2 + β3*x_3 + u
model_2 <- lm(y ~ x_1 + x_2 + x_3, data = model_test_dt)
summary(model_2)

sdc_model(model_test_dt, model_2, "id")
# on first sight:
# "no problems with dominance" message?

# model 3:
# problem dominance, no dummys
# y = β0 + β1*x_1 + β2*x_2 + β3*x_4 + u
model_3 <- lm(y ~ x_1 + x_2 + x_4, data = model_test_dt)
summary(model_3)

sdc_model(model_test_dt, model_3, "id")
# on first sight:
# "problems with dominance" message?

# model 4:
# all good, with dummys
# y = β0 + β1*x_1 + β2*x_2 + β3*dummy_1 + β4*dummy_2 + u
model_4 <- lm(y ~ x_1 + x_2 + dummy_1 + dummy_2, data = model_test_dt)
summary(model_4)

sdc_model(model_test_dt, model_4, "id")
# on first sight:
# "no problems with dominance" message?


# model 5:
# only problems with dummy (dummy_3)
# y = β0 + β1*x_1 + β2*x_2 + β3*dummy_3 + u
# dummy var trap, so exclude one dummy in dummy_3 col
model_5 <- lm(y ~ x_1 + x_2 + dummy_3, data = model_test_dt)
summary(model_5)

sdc_model(model_test_dt, model_5, "id")
# on first sight:
# "no problems with dominance" message?


# tests: return, functionality etc.


# test sdc_model ----
context("sdc_model")

# test that sdc_model returns corretly for different cases

# test that sdc_model returns TRUE, if no dummys exist
# use model 1-3
test_that("sdc_model() without dummys returns TRUE", {
    expect_equal((sdc_model(model_test_dt, model_1, "id")), TRUE)
    expect_equal((sdc_model(model_test_dt, model_2, "id")), TRUE)
    expect_equal((sdc_model(model_test_dt, model_3, "id")), TRUE)
})

# test that sdc_model returns a list, if dummys exist
# use model 4 & 5
test_that("sdc_model() with dummys returns a list", {
    expect_true(is.list(sdc_model(model_test_dt, model_4, "id")))
    expect_true(is.list(sdc_model(model_test_dt, model_5, "id")))
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


# return correct messages/output

#set up: val_2, by = "sector"
extreme_expect_5 <- function(x) {
    messages <- capture_messages(x)
    expect_match(
        paste0(messages, collapse = ""),
        paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85 ]\n",
               "[ SETTINGS: id_var: id | val_var: val_2 | by: sector ]\n",
               "It is impossible to compute extreme values for variable 'val_2' that comply to RDSC rules.",
               collapse = ""),
        fixed = TRUE
    )
}

# test that sdc_extreme returns correct messages with by argument
test_that("sdc_extreme() returns correct messages", {
    extreme_expect_4(
        sdc_extreme(extreme_test_dt_by, "id", "val", "sector"))
    extreme_expect_5(
        sdc_extreme(extreme_test_dt_by, "id", "val_2", "sector"))
})



# test für model not supported
# model finden, welches nicht supported wird
# tests für versch. Modeltypen


