# settings that make it easier to write tests
if (interactive()) {
    script <- file.path(here::here(), "tests", "testthat", "sdc_log.R")
    log <- file.path(here::here(), "tests", "testthat", "sdc_log.txt")
} else {
    script <- "sdc_log.R"
    log <- "sdc_log.txt"
}

tf <- tempfile(fileext = ".txt")
sdc_log(r_scripts = script, log_files = tf, replace = TRUE)
