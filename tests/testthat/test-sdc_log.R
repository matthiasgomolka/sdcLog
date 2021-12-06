# settings that make it easier to write tests
options(sdc.info_level = 1L)
options(data.table.print.class = FALSE)

script_1 <- list.files(pattern = "script_1.R", recursive = TRUE)
script_2 <- list.files(pattern = "script_2.R", recursive = TRUE)
script_main <- list.files(pattern = "script_main.R", recursive = TRUE)
script_error <- list.files(pattern = "script_error.R", recursive = TRUE)
log <- list.files(pattern = "test_log.txt", recursive = TRUE)

test_that(
    "sdc_log() works correctly with log files",
    # configs = "plain",
    code = {
    tf <- normalizePath(tempfile(fileext = ".txt"), mustWork = FALSE)

    msg <- ifelse(
        Sys.info()[["sysname"]] == "Windows",
        "Log file for",
        paste0("Log file for '.*script_1.R' written to '.*", tf, "'.")
    )
    expect_message(
        sdc_log(r_script = script_1, destination = tf),
        msg
    )
    exclude <- c(8, 29)
    expect_identical(readLines(tf)[-exclude], gsub("✓", "v", readLines(log))[-exclude])
    expect_match(readLines(tf)[exclude], "Output complies to RDC rules.")
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
    exclude <- c(8, 29)
    expect_identical(readLines(tf_conn)[-exclude], gsub("✓", "v", readLines(log))[-exclude])
    expect_match(readLines(tf_conn)[exclude], "Output complies to RDC rules.")
})

test_that("sdc_log() handles nested calls to sdc_log()", {
    tf_conn <- tempfile(fileext = ".txt")
    conn <- file(tf_conn, encoding = "UTF-8", open = "w")

    expect_message(
        sdc_log(r_script = script_main, destination = conn, local = environment()),
        paste0("Log file for '.*script_main.R' written to 'file connection'.")
    )

    close(conn)
    log_main_1 <- c(
        "",
        "> # First script",
        "> sdc_log(script_1, conn, append = TRUE)"
    )
    log_main_2 <- c(
        "",
        "> # random content",
        "> 1 + 1",
        "[1] 2",
        "",
        "> # Second script",
        "> sdc_log(script_2, conn, append = TRUE)"
    )

    actual <- readLines(tf_conn)
    actual <- actual[grep("Output complies|^(--|──)", actual, invert = TRUE)]

    expected <- c(
        log_main_1,
        gsub("✓", "v", readLines(log)),
        log_main_2,
        gsub("✓", "v", readLines(log))
    )
    expected <- expected[grep("Output complies|^(--|──)", expected, invert = TRUE)]

    # For some reason, comments from script_main disappear in the testing
    # environment. This only happens when I run the tests using "Run Tests" or
    # "Test Package", not in the interactive session.
    expect_vector(
        setdiff(actual, expected),
        ptype = character(),
        size = 0
    )
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
        "Assertion on 'r_script' failed: File extension must be in {'R'}",
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
        Sys.info()[["sysname"]] == "Windows",
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


test_that("sdc_log() can be called from function", {
    tf_in <- tempfile(fileext = ".R")
    tf_out <- tempfile()

    writeLines("print(bar)", tf_in)

    foo <- function() {
        bar <- "calling environment variable"
        sdc_log(tf_in, tf_out, append = TRUE, local = environment())
    }

    expected <- paste0("Log file for '", tf_in, "' written to '", tf_out, "'.")
    output <- expect_message(foo(), expected, fixed = TRUE)

    expect_identical(
        readLines(tf_out),
        c("", "> print(bar)", "[1] \"calling environment variable\"")
    )
})
