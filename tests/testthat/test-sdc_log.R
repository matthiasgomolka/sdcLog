# tests sdc_log
library(testthat)

# test set up
# new script: sdc_test_log.R

# create log file to compare
# need to replace path - here::here()?
sink(file = "C:/sdcLog/data-raw/sdc_test_log2.txt")
tryCatch(
    source(
        "C:/sdcLog/data-raw/sdc_test_log.R",
        echo = TRUE,
        continue.echo = "+ ",
        skip.echo = 0,
        max.deparse.length = Inf,
        chdir = FALSE
    ))
sink()

context("sdc_log")
# test that sdc_log works correctly
test_that("sdc_log() works correctly", {
    sdc_log(r_scripts = "C:/sdcLog/data-raw/sdc_test_log.R",
            log_files = "C:/sdcLog/data-raw/sdc_test_log.txt", replace = TRUE)
    expect_equal(tools::Rdiff("C:/sdcLog/data-raw/sdc_test_log.txt", "C:/sdcLog/data-raw/sdc_test_log2.txt"), 0L)
})


# test that sdc_log returns appropriate error
test_that("sdc_log() returns appropriate error", {

    # error für existing log_file
    expect_error(sdc_log(r_scripts = "C:/sdcLog/data-raw/sdc_test_log.R",
                         log_files = "C:/sdcLog/data-raw/sdc_test_log.txt"),
                 "At least one log file in 'log_files' already exists. Please check 'log_files' argument or use 'replace = TRUE' in case you want to replace existing files.", fixed = TRUE
    )

    # error für nichtexistierende Elemente
    expect_error(sdc_log(r_scripts = "C:/sdcLog/data-raw/sdc_wrong_test_log.R",
                         log_files = "C:/sdcLog/data-raw/sdc_test_log.txt"), "Assertion on 'r_scripts' failed: File does not exist: 'C:/sdcLog/data-raw/sdc_wrong_test_log.R'.")
    expect_error(sdc_log(r_scripts = "C:/sdcLog/data-raw/sdc_test_log.txt",
                         log_files = "C:/sdcLog/data-raw/sdc_test_log.txt"), "Assertion on 'r_scripts' failed: File extension must be in {'R'}.", fixed = TRUE)

    # error für elements unquoted
    expect_error(sdc_log(r_scripts = file,
                         log_files = "C:/sdcLog/data-raw/sdc_test_log.txt"), "Assertion on 'r_scripts' failed: Must be of type 'character', not 'closure'.")
    expect_error(sdc_log(r_scripts = "C:/sdcLog/data-raw/sdc_test_log.R",
                         log_files = file), "Assertion on 'log_files' failed: Must be of type 'character', not 'closure'.")

    # error für missing arguments
    expect_error(sdc_log(log_files = "C:/sdcLog/data-raw/sdc_test_log.txt"), "argument \"r_scripts\" is missing, with no default")
    expect_error(sdc_log(r_scripts = "C:/sdcLog/data-raw/sdc_test_log.R"), "argument \"log_files\" is missing, with no default")

    # error für silent not logical
    expect_error(sdc_log(r_scripts = "C:/sdcLog/data-raw/sdc_test_log.R",
            log_files = "C:/sdcLog/data-raw/sdc_test_log.txt", silent = yes), "object 'yes' not found")

    # error für unterschiedl. Länge
    expect_error(sdc_log(r_scripts = c("C:/sdcLog/data-raw/sdc_test_log.R", "C:/sdcLog/data-raw/create_sdc_DT.R"),
                         log_files = "C:/sdcLog/data-raw/sdc_test_log.txt"),
                 "Assertion on 'length(r_scripts)' failed: Must be equal to set {'1'}, but is {'2'}", fixed = TRUE)

    })



