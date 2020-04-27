#' Create Stata-like log files from R Scripts
#' @description This function creates Stata-like log files from R Scripts. It
#'   can handle single files or a list of files (in a character vector) at once.
#' @param r_scripts character vector containing the path(s) of the R script(s)
#'   which should be run with logging
#' @param log_files character vector containing the path(s) of the text file(s)
#'   where the log(s) should be stored
#' @param replace logical. Indicates whether to replace existing log files.
#' @param silent logical. Switch to TRUE in order to receive warnings and
#'   messages from running the 'r_scripts'.
#' @return Invisible \code{NULL}.
#' @importFrom tools file_ext
#' @export

sdc_log <- function(r_scripts, log_files, replace = FALSE, silent = TRUE) {
  # check inputs
  # rewritten using checkmate

  checkmate::assert_character(r_scripts)
  checkmate::assert_character(log_files)
  checkmate::assert_logical(silent)
  checkmate::assert_set_equal(length(r_scripts), length(log_files))
  checkmate::assert_file(r_scripts, extension = "R")

  if (!replace) {
        if (any(checkmate::checkFile(log_files)))
          stop("At least one log file in 'log_files' already exists. Please check 'log_files' argument or use 'replace = TRUE' in case you want to replace existing files.")
      }

  #if (!is.character(r_scripts))
  #stop("Argument 'r_scripts' must be of type 'character'.")

  #if (!is.character(log_files))
    #stop("Argument 'log_files' must be of type 'character'.")

  #if (!is.logical((silent)))
   # stop("Argument 'silent' must be of type 'logical'.")

  #if (length(r_scripts) != length(log_files))
    #stop("Arguments 'r_scripts' and 'log_files' must be of the same length.")

  #if (any(!file.exists(r_scripts)))
   # stop("At least one input file in 'r_scripts' does not exist.")

  #if (any(tools::file_ext(r_scripts) != "R"))
   # stop("At least one input file in 'r_scripts' is not an R script, e.g. does not end with '.R'.")

  #if (!replace) {
  #  if (any(file.exists(log_files)))
  #    stop("At least one log file in 'log_files' already exists. Please check 'log_files' argument or use 'replace = TRUE' in case you want to replace existing files.")
  #}

  # write log
  # wir könnten eine Option für parallel processing ergänzen
  if (silent) {
    suppressWarnings(
      suppressMessages(
        mapply(generate_log, r_scripts, log_files)
      )
    )
  } else {
    mapply(generate_log, r_scripts, log_files)
  }
}


#' Source a single R script and generate a log file
#' @param r_script R script to be run and logged
#' @param log_file destination file of the log file to be generated
#' @return NULL

generate_log <- function(r_script, log_file) {
  sink(file = log_file)
  tryCatch(
    source(
      r_script,
      echo = TRUE,
      continue.echo = "+ ",
      skip.echo = 0,
      max.deparse.length = Inf,
      chdir = FALSE
    ),
    # without error handling, further output would also be written to the log
    # file
    error = function(x) {
      sink(file = NULL)
      stop(
        "An error occurred while running the script '", r_script,
        "'.\nLog will be incomplete!",
        call. = FALSE
      )
    }
  )
  if (log_file %in% showConnections()[, 1]) {
    sink(file = NULL)
    message("Log file for '", r_script, "' written to '", log_file, "'.\n\n")
  }
}
