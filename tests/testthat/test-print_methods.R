 library(data.table)


# test print.sdc_distinct_ids ----
context("print.sdc_distinct_ids")
distinct_ids_1 <- data.table(distinct_ids = integer(0L))

 test_that("print.sdc_distinct_ids works for most simple case", {
     options(sdc.info_level = 0L)
     expect_silent(sdcLog:::print.sdc_distinct_ids(distinct_ids_1))

     options(sdc.info_level = 1L)
     expect_silent(sdcLog:::print.sdc_distinct_ids(distinct_ids_1))

     options(sdc.info_level = 2L)
     expect_message(
         sdcLog:::print.sdc_distinct_ids(distinct_ids_1),
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
     expect_print.sdc_distinct_ids_2(sdcLog:::print.sdc_distinct_ids(distinct_ids_2))

     options(sdc.info_level = 1L)
     expect_print.sdc_distinct_ids_2(sdcLog:::print.sdc_distinct_ids(distinct_ids_2))

     options(sdc.info_level = 2L)
     expect_print.sdc_distinct_ids_2(sdcLog:::print.sdc_distinct_ids(distinct_ids_2))
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
     expect_print.sdc_distinct_ids_3(sdcLog:::print.sdc_distinct_ids(distinct_ids_3))

     options(sdc.info_level = 1L)
     expect_print.sdc_distinct_ids_3(sdcLog:::print.sdc_distinct_ids(distinct_ids_3))

     options(sdc.info_level = 2L)
     expect_print.sdc_distinct_ids_3(sdcLog:::print.sdc_distinct_ids(distinct_ids_3))
 })


# test print.sdc_dominance ----
context("print.sdc_dominance")

dominance_1 <- data.table(value_share = numeric(0))

test_that("print.sdc_dominance works for most simple case", {
     options(sdc.info_level = 0L)
     expect_silent(sdcLog:::print.sdc_dominance(dominance_1))

     options(sdc.info_level = 1L)
     expect_silent(sdcLog:::print.sdc_dominance(dominance_1))

     options(sdc.info_level = 2L)
     expect_message(
         sdcLog:::print.sdc_dominance(dominance_1),
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
     expect_print.sdc_dominance_2(sdcLog:::print.sdc_dominance(dominance_2))

     options(sdc.info_level = 1L)
     expect_print.sdc_dominance_2(sdcLog:::print.sdc_dominance(dominance_2))

     options(sdc.info_level = 2L)
     expect_print.sdc_dominance_2(sdcLog:::print.sdc_dominance(dominance_2))
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
     expect_print.sdc_dominance_3(sdcLog:::print.sdc_dominance(dominance_3))

     options(sdc.info_level = 1L)
     expect_print.sdc_dominance_3(sdcLog:::print.sdc_dominance(dominance_3))

     options(sdc.info_level = 2L)
     expect_print.sdc_dominance_3(sdcLog:::print.sdc_dominance(dominance_3))
 })


# test print.sdc_descriptives ----
context("print.sdc_descriptives")
descriptives_1 <- list(distinct_ids = distinct_ids_1, dominance = dominance_1)
class(descriptives_1[["distinct_ids"]]) <-
    c("sdc_distinct_ids", class(descriptives_1[["distinct_ids"]]))
class(descriptives_1[["dominance"]]) <-
    c("sdc_dominance", class(descriptives_1[["dominance"]]))
class(descriptives_1) <- c("sdc_descriptives", class(descriptives_1))

test_that("print.sdc_descriptives works for most simple case", {
     options(sdc.info_level = 0L)
     expect_silent(sdcLog:::print.sdc_descriptives(descriptives_1))

     options(sdc.info_level = 1L)
     expect_message(
         sdcLog:::print.sdc_descriptives(descriptives_1),
         "Output complies to RDSC rules.")

     options(sdc.info_level = 2L)
     expect_message(
         sdcLog:::print.sdc_descriptives(descriptives_1),
         "Output complies to RDSC rules.",
         fixed = TRUE
     )
 })

descriptives_2 <- list(distinct_ids = distinct_ids_2, dominance = dominance_2)
class(descriptives_2[["distinct_ids"]]) <-
    c("sdc_distinct_ids", class(descriptives_2[["distinct_ids"]]))
class(descriptives_2[["dominance"]]) <-
    c("sdc_dominance", class(descriptives_2[["dominance"]]))
class(descriptives_2) <- c("sdc_descriptives", class(descriptives_2))

test_that("print.sdc_descriptives works for problematic case", {
     options(sdc.info_level = 0L)
     expect_silent(sdcLog:::print.sdc_descriptives(descriptives_2))

     options(sdc.info_level = 1L)
     expect_silent(sdcLog:::print.sdc_descriptives(descriptives_2))

     options(sdc.info_level = 2L)
     expect_silent(sdcLog:::print.sdc_descriptives(descriptives_2))
 })

descriptives_3 <- list(distinct_ids = distinct_ids_3, dominance = dominance_3)
class(descriptives_3[["distinct_ids"]]) <-
    c("sdc_distinct_ids", class(descriptives_3[["distinct_ids"]]))
class(descriptives_3[["dominance"]]) <-
    c("sdc_dominance", class(descriptives_3[["dominance"]]))
class(descriptives_3) <- c("sdc_descriptives", class(descriptives_3))

test_that("print.sdc_descriptives works for problematic case", {
     options(sdc.info_level = 0L)
     expect_silent(sdcLog:::print.sdc_descriptives(descriptives_3))

     options(sdc.info_level = 1L)
     expect_silent(sdcLog:::print.sdc_descriptives(descriptives_3))

     options(sdc.info_level = 2L)
     expect_silent(sdcLog:::print.sdc_descriptives(descriptives_3))
 })



# test print.sdc_model ----
context("print.sdc_model")

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


model_ref_1 <- list(message_options = message_options(),
                    message_arguments = message_arguments(id_var = "id"),
                    distinct_ids = distinct_ids_1,
                    dominance_list = dominance_list_1,
                    dummy_list = dummy_list_1)

class(model_ref_1) <- c("sdc_model", class(model_ref_1))

test_that("print.sdc_model works for most simple case", {
    options(sdc.info_level = 0L)
    messages <- capture_messages(sdcLog:::print.sdc_model(model_ref_1))
    expect_match(
        paste0(messages, collapse = ""),
        paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | ",
               "sdc.share_dominance: 0.85 ]\n",
               "[ SETTINGS: id_var: id ]\n",
               collapse = ""),
            fixed = TRUE)

    options(sdc.info_level = 1L)
    messages <- capture_messages(sdcLog:::print.sdc_model(model_ref_1))
    expect_match(
        paste0(messages, collapse = ""),
        paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | ",
               "sdc.share_dominance: 0.85 ]\n",
               "[ SETTINGS: id_var: id ]\n",
               "Output complies to RDSC rules.\n",
               collapse = ""),
            fixed = TRUE)

    options(sdc.info_level = 2L)
    messages <- capture_messages(sdcLog:::print.sdc_model(model_ref_1))
    expect_match(
        paste0(messages, collapse = ""),
        paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | ",
               "sdc.share_dominance: 0.85 ]\n",
               "[ SETTINGS: id_var: id ]\n",
               "No problem with number of distinct entities.\n",
               "No problem with dominance.\n",
               "No problem with dominance.\n",
               "No problem with dominance.\n",
               "Output complies to RDSC rules.\n",
               collapse = ""),
            fixed = TRUE)
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
dummy_1 <- data.table(dummy_1 = "S",
                      distinct_ids = 4)
class(dummy_1) <- c("sdc_distinct_ids", class(dummy_1))
dummy_list_2 <- list(dummy_1)
dummy_vars_2 <- "dummy_1"
names(dummy_list_2) <- dummy_vars_2


model_ref_2 <- list(message_options = message_options(),
                    message_arguments = message_arguments(id_var = "id"),
                    distinct_ids = distinct_ids_2,
                    dominance_list = dominance_list_2,
                    dummy_list = dummy_list_2)

class(model_ref_2) <- c("sdc_model", class(model_ref_2))

# würde so funktionieren, wenn in print_methods nicht cat gecallt würde
test_that("print.sdc_model works for problematic cases", {
    options(sdc.info_level = 0L)
    messages <- capture_messages(sdcLog:::print.sdc_model(model_ref_2))
    expect_match(
        paste0(messages, collapse = ""),
        paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | ",
               "sdc.share_dominance: 0.85 ]\n",
               "[ SETTINGS: id_var: id ]\n",
               "Not enough distinct entities:\n",
               "Dominant entities:\n",
               "Dominant entities:\n",
               "Not enough distinct entities:\n",
               collapse = ""),
        fixed = TRUE)


    options(sdc.info_level = 1L)
    messages <- capture_messages(sdcLog:::print.sdc_model(model_ref_2))
    expect_match(
        paste0(messages, collapse = ""),
        paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | ",
               "sdc.share_dominance: 0.85 ]\n",
               "[ SETTINGS: id_var: id ]\n",
               "Not enough distinct entities:\n",
               "Dominant entities:\n",
               "Dominant entities:\n",
               "Not enough distinct entities:\n",
               collapse = ""),
        fixed = TRUE)


    options(sdc.info_level = 2L)
    messages <- capture_messages(sdcLog:::print.sdc_model(model_ref_2))
    expect_match(
        paste0(messages, collapse = ""),
        paste0("[ OPTIONS:  sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | ",
               "sdc.share_dominance: 0.85 ]\n",
               "[ SETTINGS: id_var: id ]\n",
               "Not enough distinct entities:\n",
               "Dominant entities:\n",
               "Dominant entities:\n",
               "Not enough distinct entities:\n",
               collapse = ""),
        fixed = TRUE)

})



# distinct_ids_3 for problematic by case

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


## complex problematic cases (some variables lead to problems)


