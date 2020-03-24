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
    x_2 = jitter(y, factor = 5000),
    x_3 = jitter(y, factor = 50000),
    x_4 = jitter(y, factor = 60000),
    dummy_1 = sort(rep_len(paste0("M", 1L:2L), n)),
    dummy_2 = sort(rep_len(paste0("Y", 1L:10L), n)),
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
# only problems with dummys
# y = β0 + β1*x_1 + β2*x_2 + β3*dummy_1 + β4*dummy_2 + β4*dummy_3 + u
model_5 <- lm(y ~ x_1 + x_2 + dummy_1 + dummy_2 + dummy_3, data = model_test_dt)
summary(model_5)
sdc_model(model_test_dt, model_5, "id")

# tests: return, functionality etc.

# test für model not supported
# model finden, welches nicht supported wird
# tests für versch. Modeltypen


