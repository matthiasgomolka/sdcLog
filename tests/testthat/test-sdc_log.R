# settings that make it easier to write tests
options(sdc.info_level = 1L)
options(data.table.print.class = FALSE)

script_1 <- list.files(pattern = "script_1.R", recursive = TRUE)
script_2 <- list.files(pattern = "script_2.R", recursive = TRUE)
script_main <- list.files(pattern = "script_main.R", recursive = TRUE)
script_error <- list.files(pattern = "script_error.R", recursive = TRUE)
log <- list.files(pattern = "test_log.txt", recursive = TRUE)

test_that("sdc_log() works correctly with log files", {
  tf <- normalizePath(tempfile(fileext = ".txt"), mustWork = FALSE)

  msg <- ifelse(
    Sys.info()[['sysname']] == "Windows",
    "Log file for",
    paste0("Log file for '.*script_1.R' written to '.*", tf, "'.")
  )
  expect_message(
    sdc_log(r_script = script_1, destination = tf),
    msg
  )
  expect_identical(readLines(tf), readLines(log))
})

test_that("sdc_log() works correctly with connections", {
  expect_identical(sink.number(), 0L)

  tf_conn <- tempfile(fileext = ".txt")

  conn <- file(tf_conn, encoding = "UTF-8", open = "w")

  expect_message(
    sdc_log(r_script = script_1, destination = conn),
    paste0("Log file for '.*script_1.R' written to 'file connection'.")
  )
  expect_identical(sink.number(), 0L)

  close(conn)
  expect_identical(readLines(tf_conn), readLines(log))
})

test_that("sdc_log() handles nested calls to sdc_log()", {
  skip_if_not(
    interactive(),
    "This somehow does not work (yet) in the temporary test environment. But does interactively!"
  )
  tf_conn <- tempfile(fileext = ".txt")
  conn <- file(tf_conn, encoding = "UTF-8", open = "w")

  expect_message(
    sdc_log(r_script = script_main, destination = conn),
    paste0("Log file for '.*script_main.R' written to 'file connection'.")
  )

  close(conn)
  log_main_1 <- c(
    "",
    "> # First script",
    '> sdc_log(script_1, conn, append = TRUE)'
  )
  log_main_2 <- c(
    "",
    "> # random content",
    "> 1 + 1",
    "[1] 2",
    "",
    "> # Second script",
    '> sdc_log(script_2, conn, append = TRUE)'
  )

  actual <- readLines(tf_conn)

  expected <- c(
    log_main_1,
    readLines(log),
    log_main_2,
    readLines(log)
  )

  expect_identical(actual, expected)
})

test_that("sdc_log() returns appropriate error", {

  # error for existing log_file
  expect_error(
    sdc_log(r_script = script_1, destination = log),
    paste0(
      "'destination' already exists. Please check 'destination' or use ",
      "'replace = TRUE' / append = TRUE in case you want to replace / append ",
      "the existing file."
    ),
    fixed = TRUE
  )

  # error for non-existing / wrong scripts
  expect_error(
    sdc_log(r_script = "doesnotexist.R", destination = log),
    "Assertion on 'r_script' failed: File does not exist: 'doesnotexist.R'.",
    fixed = TRUE
  )

  tf <- tempfile(fileext = ".RScript")
  file.copy(from = script_1, to = tf)
  expect_error(
    sdc_log(r_script = tf, destination = log),
    "Assertion on 'r_script' failed: File extension must be in {'R'}.",
    fixed = TRUE
  )

  # error for inactive connection
  conn <- file(tempfile(), encoding = "UTF-8", open = "w")
  close(conn)
  expect_error(
    sdc_log(r_script = script_1, destination = conn),
    "The connection provided in 'destination' is not active.",
    fixed = TRUE
  )
})

test_that("error in script is handled correctly", {
  expect_identical(sink.number(), 0L)
  expect_identical(sink.number("message"), 2L)

  tf <- normalizePath(tempfile(fileext = ".txt"), mustWork = FALSE)

  msg <- ifelse(
    Sys.info()[['sysname']] == "Windows",
    "Log file for",
    paste0("Log file for '.*script_error.R' written to '.*", tf, "'.")
  )

  expect_message(
    expect_warning(
      sdc_log(script_error, tf),
      paste0(
        "An error occured during the execution of '", script_error, "'. ",
        "The log file will be incomplete."
      )
    ),
    msg
  )

  expect_identical(sink.number(), 0L)
  expect_identical(sink.number("message"), 2L)
})
