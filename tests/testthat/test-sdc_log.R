# settings that make it easier to write tests
if (interactive()) {
  script <- file.path(here::here(), "tests", "testthat", "sdc_log.R")
  log <- file.path(here::here(), "tests", "testthat", "sdc_log.txt")
} else {
  script <- "sdc_log.R"
  log <- "sdc_log.txt"
}

tf <- tempfile(fileext = ".txt")

invisible(
  callr::r(
    function(...) {
      library(sdcLog)
      sdc_log(...)
    },
    args = list(r_scripts = script, log_files = tf)
  )
)

test_that("sdc_log() works correctly", {
  expect_identical(readLines(tf), readLines(log))
})


test_that("sdc_log() returns appropriate error", {

  # error for existing log_file
  expect_error(
    sdc_log(r_scripts = script, log_files = log),
    paste0(
      "The following 'log_files' already exist:\n",
      log,
      "\nPlease check 'log_files' argument or use 'replace = TRUE' in case ",
      "you want to replace existing files."
    ),
    fixed = TRUE
  )

  # error for duplicate script
  expect_error(
    sdc_log(r_scripts = c(script, script), log_files = c(log, tempfile())),
    "Assertion on 'r_scripts' failed: Contains duplicated values, position 2.",
    fixed = TRUE
  )

  # error for duplicate log
  tfR <- tempfile(fileext = ".R")
  file.copy(from = script, to = tfR)
  expect_error(
    sdc_log(r_scripts = c(script, tfR), log_files = c(log, log)),
    "Assertion on 'log_files' failed: Contains duplicated values, position 2.",
    fixed = TRUE
  )


  # error for non-existing / wrong scripts
  expect_error(
    sdc_log(r_scripts = "doesnotexist.R", log_files = log),
    "Assertion on 'r_scripts' failed: File does not exist: 'doesnotexist.R'.",
    fixed = TRUE
  )

  tf <- tempfile(fileext = ".RScript")
  file.copy(from = script, to = tf)
  expect_error(
    sdc_log(r_scripts = tf, log_files = log),
    "Assertion on 'r_scripts' failed: File extension must be in {'R'}.",
    fixed = TRUE
  )


  # error for different number of scripts and logs
  expect_error(
    sdc_log(r_scripts = c(script, tfR), log_files = log),
    "Assertion on 'log_files' failed: Must have length 2, but has length 1.",
    fixed = TRUE
  )
})
