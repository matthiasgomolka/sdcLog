#' Create Stata-like log files from R Scripts
#' @description This function creates Stata-like log files from R Scripts. It
#'   can handle several files (in a [character] vector) at once.
#' @param r_scripts [character] vector containing the path(s) of the R script(s)
#'   which should be run with logging.
#' @param log_files [character] vector containing the path(s) of the text
#'   file(s) where the log(s) should be stored.
#' @param replace [logical] Indicates whether to replace existing log files.
#' @return Invisible `NULL`.
#' @importFrom checkmate assert_character assert_logical assert_file
#'   test_file_exists
#' @export
sdc_log <- function(r_scripts, log_files, replace = FALSE) {
  # check inputs
  checkmate::assert_character(r_scripts, unique = TRUE)
  checkmate::assert_character(log_files, unique = TRUE, len = length(r_scripts))
  checkmate::assert_file(r_scripts, extension = "R")

  if (isFALSE(replace)) {
    exist <- checkmate::test_file_exists(log_files)
    if (any(exist)) {
      existing_files <- paste0(log_files[exist], collapse = "\n")
      stop(
        "The following 'log_files' already exist:\n",
        existing_files, "\n",
        "Please check 'log_files' argument or use 'replace = TRUE' in case ",
        "you want to replace existing files."
      )
    }
  }

  # write log
  # TODO: Option for running scripts in parallel
  invisible(mapply(generate_log, r_scripts, log_files, USE.NAMES = FALSE))

  # message("Log(s) written to\n", paste0(log_files, collapse = "\n"))
}


#' Source a single R script and generate a log file
#' @param r_script R script to be run and logged
#' @param log_file destination file of the log file to be generated
#' @return NULL

generate_log <- function(r_script, log_file) {
  file.create(log_file)
  conn <- file(log_file, open = "w", encoding = "UTF-8")
  sink(file = conn, append = TRUE)
  sink(file = conn, append = TRUE, type = "message")

  # make sure that outputs is printed to console, even if this function stopped
  on.exit({
    suppressWarnings({
      sink(type = "output")
      sink(type = "message")
    })
    close(conn)
  })

  source(
    r_script,
    echo = TRUE,
    continue.echo = "+ ",
    skip.echo = 0,
    max.deparse.length = Inf,
    width.cutoff = 80,
    chdir = FALSE
  )


  sink(type = "output")
  sink(type = "message")

  message("Log file for '", r_script, "' written to '", log_file, "'.\n\n")
  return(log_file)
}
