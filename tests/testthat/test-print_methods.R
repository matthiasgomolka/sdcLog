# library(data.table)
#
#
# # test print.sdc_distinct_ids ----
# context("print.sdc_distinct_ids")
# distinct_ids_1 <- data.table(distinct_ids = integer(0L))
#
# test_that("print.sdc_distinct_ids works for most simple case", {
#     options(sdc.info_level = 0L)
#     expect_silent(sdcLog:::print.sdc_distinct_ids(distinct_ids_1))
#
#     options(sdc.info_level = 1L)
#     expect_silent(sdcLog:::print.sdc_distinct_ids(distinct_ids_1))
#
#     options(sdc.info_level = 2L)
#     expect_message(
#         sdcLog:::print.sdc_distinct_ids(distinct_ids_1),
#         "No problem with number of distinct entities.",
#         fixed = TRUE
#     )
# })
#
# distinct_ids_2 <- data.table(distinct_ids = 3L)
# expect_print.sdc_distinct_ids_2 <- function(x) {
#     expect_warning(
#         expect_output(
#             expect_identical(x, distinct_ids_2),
#             "Not enough distinct entities:\n",
#             fixed = TRUE
#         ),
#         "Potential disclosure problem: Not enough distinct entities.",
#         fixed = TRUE
#     )
# }
#
# test_that("print.sdc_distinct_ids works for problematic case", {
#     options(sdc.info_level = 0L)
#     expect_print.sdc_distinct_ids_2(sdcLog:::print.sdc_distinct_ids(distinct_ids_2))
#
#     options(sdc.info_level = 1L)
#     expect_print.sdc_distinct_ids_2(sdcLog:::print.sdc_distinct_ids(distinct_ids_2))
#
#     options(sdc.info_level = 2L)
#     expect_print.sdc_distinct_ids_2(sdcLog:::print.sdc_distinct_ids(distinct_ids_2))
# })
#
# distinct_ids_3 <- data.table(sector = paste0("S", 1:2), distinct_ids = 3L:4L)
# expect_print.sdc_distinct_ids_3 <- function(x) {
#     expect_warning(
#         expect_output(
#             expect_identical(x, distinct_ids_3),
#             "Not enough distinct entities:\n",
#             fixed = TRUE
#         ),
#         "Potential disclosure problem: Not enough distinct entities.",
#         fixed = TRUE
#     )
# }
#
# test_that("print.sdc_distinct_ids works for problematic by case", {
#     options(sdc.info_level = 0L)
#     expect_print.sdc_distinct_ids_3(sdcLog:::print.sdc_distinct_ids(distinct_ids_3))
#
#     options(sdc.info_level = 1L)
#     expect_print.sdc_distinct_ids_3(sdcLog:::print.sdc_distinct_ids(distinct_ids_3))
#
#     options(sdc.info_level = 2L)
#     expect_print.sdc_distinct_ids_3(sdcLog:::print.sdc_distinct_ids(distinct_ids_3))
# })
#
#
# # test print.sdc_dominance ----
# context("print.sdc_dominance")
#
# dominance_1 <- data.table(value_share = numeric(0))
#
# test_that("print.sdc_dominance works for most simple case", {
#     options(sdc.info_level = 0L)
#     expect_silent(sdcLog:::print.sdc_dominance(dominance_1))
#
#     options(sdc.info_level = 1L)
#     expect_silent(sdcLog:::print.sdc_dominance(dominance_1))
#
#     options(sdc.info_level = 2L)
#     expect_message(
#         sdcLog:::print.sdc_dominance(dominance_1),
#         "No problem with dominance.",
#         fixed = TRUE
#     )
# })
#
# dominance_2 <- data.table(value_share = 0.9)
# expect_print.sdc_dominance_2 <- function(x) {
#     expect_warning(
#         expect_output(
#             expect_identical(x, dominance_2),
#             "Dominant entities:\n",
#             fixed = TRUE
#         ),
#         "Potential disclosure problem: Dominant entities.",
#         fixed = TRUE
#     )
# }
#
# test_that("print.sdc_dominance works for problematic case", {
#     options(sdc.info_level = 0L)
#     expect_print.sdc_dominance_2(sdcLog:::print.sdc_dominance(dominance_2))
#
#     options(sdc.info_level = 1L)
#     expect_print.sdc_dominance_2(sdcLog:::print.sdc_dominance(dominance_2))
#
#     options(sdc.info_level = 2L)
#     expect_print.sdc_dominance_2(sdcLog:::print.sdc_dominance(dominance_2))
# })
#
# dominance_3 <- data.table(sector = paste0("S", 1:2), value_share = c(0.88, 0.9))
# expect_print.sdc_dominance_3 <- function(x) {
#     expect_warning(
#         expect_output(
#             expect_identical(x, dominance_3),
#             "Dominant entities:\n",
#             fixed = TRUE
#         ),
#         "Potential disclosure problem: Dominant entities.",
#         fixed = TRUE
#     )
# }
#
# test_that("print.sdc_dominance works for problematic by case", {
#     options(sdc.info_level = 0L)
#     expect_print.sdc_dominance_3(sdcLog:::print.sdc_dominance(dominance_3))
#
#     options(sdc.info_level = 1L)
#     expect_print.sdc_dominance_3(sdcLog:::print.sdc_dominance(dominance_3))
#
#     options(sdc.info_level = 2L)
#     expect_print.sdc_dominance_3(sdcLog:::print.sdc_dominance(dominance_3))
# })
#
#
# # test print.sdc_descriptives ----
# context("print.sdc_descriptives")
# descriptives_1 <- list(distinct_ids = distinct_ids_1, dominance = dominance_1)
# class(descriptives_1[["distinct_ids"]]) <-
#     c("sdc_distinct_ids", class(descriptives_1[["distinct_ids"]]))
# class(descriptives_1[["dominance"]]) <-
#     c("sdc_dominance", class(descriptives_1[["dominance"]]))
# class(descriptives_1) <- c("sdc_descriptives", class(descriptives_1))
#
# test_that("print.sdc_descriptives works for most simple case", {
#     options(sdc.info_level = 0L)
#     expect_silent(sdcLog:::print.sdc_descriptives(descriptives_1))
#
#     options(sdc.info_level = 1L)
#     expect_message(
#         sdcLog:::print.sdc_descriptives(descriptives_1),
#         "Output complies to RDSC rules.")
#
#     options(sdc.info_level = 2L)
#     expect_message(
#         sdcLog:::print.sdc_descriptives(descriptives_1),
#         "Output complies to RDSC rules.",
#         fixed = TRUE
#     )
# })
#
# descriptives_2 <- list(distinct_ids = distinct_ids_2, dominance = dominance_2)
# class(descriptives_2[["distinct_ids"]]) <-
#     c("sdc_distinct_ids", class(descriptives_2[["distinct_ids"]]))
# class(descriptives_2[["dominance"]]) <-
#     c("sdc_dominance", class(descriptives_2[["dominance"]]))
# class(descriptives_2) <- c("sdc_descriptives", class(descriptives_2))
#
# test_that("print.sdc_descriptives works for problematic case", {
#     options(sdc.info_level = 0L)
#     expect_silent(sdcLog:::print.sdc_descriptives(descriptives_2))
#
#     options(sdc.info_level = 1L)
#     expect_silent(sdcLog:::print.sdc_descriptives(descriptives_2))
#
#     options(sdc.info_level = 2L)
#     expect_silent(sdcLog:::print.sdc_descriptives(descriptives_2))
# })
#
# descriptives_3 <- list(distinct_ids = distinct_ids_3, dominance = dominance_3)
# class(descriptives_3[["distinct_ids"]]) <-
#     c("sdc_distinct_ids", class(descriptives_3[["distinct_ids"]]))
# class(descriptives_3[["dominance"]]) <-
#     c("sdc_dominance", class(descriptives_3[["dominance"]]))
# class(descriptives_3) <- c("sdc_descriptives", class(descriptives_3))
#
# test_that("print.sdc_descriptives works for problematic case", {
#     options(sdc.info_level = 0L)
#     expect_silent(sdcLog:::print.sdc_descriptives(descriptives_3))
#
#     options(sdc.info_level = 1L)
#     expect_silent(sdcLog:::print.sdc_descriptives(descriptives_3))
#
#     options(sdc.info_level = 2L)
#     expect_silent(sdcLog:::print.sdc_descriptives(descriptives_3))
# })
#
#
#
# #test print.sdc_model ----
# context("print.sdc_model")
