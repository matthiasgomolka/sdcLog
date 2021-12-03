#' Create Stata-like log files from R Scripts
#' @description This function creates Stata-like log files from R Scripts. It
#'   can handle several files (in a [character] vector) at once.
#' @param r_script [character] Path of the R script to be run with logging.
#' @param destination One of:
#'
#'   - [character] Path of the log file to be used.
#'   - [file] connection to which the log should be written. This is especially
#'     useful, when you have nested calls to `sdc_log()` and want to write
#'     everything into the same log file. Then, create a single [file]
#'     connection and provide this connection to all calls to `sdc_log()` (and
#'     close it afterwards).
#' @param replace [logical] Indicates whether to replace an existing log file.
#' @param append [logical] Indicates whether to append an existing log file.
#' @param local One of:
#'
#'   - [logical] Indicates whether to evaluate within the global environment
#'     (`FALSE`) or the calling environment (`TRUE`).
#'   - [environment] A specific evaluation environment. Determines the
#'     evaluation environment. Useful whenever `sdc_log()` is called from within
#'     a function, or for nested `sdc_log()` calls. By default (`FALSE`)
#'     evaluation occurs in the global environment. See also [source].
#' @return [character] vector holding the path(s) of the written log file(s).
#' @importFrom checkmate assert_character assert_logical assert_file
#'   test_file_exists
#' @export
sdc_log <- function(
  r_script, destination, replace = FALSE, append = FALSE, local = FALSE
) {

  # check inputs
  checkmate::assert_string(r_script)
  checkmate::assert_file(r_script, extension = "R")
  is_conn <- inherits(destination, c("file", "connection"))
  checkmate::assert_true(any(
    checkmate::test_string(destination),
    is_conn
  ))
  if (is_conn) {
    conn <- tryCatch(
      getConnection(destination),
      error = function(error) stop(
        "The connection provided in 'destination' is not active.",
        call. = FALSE
      )
    )
  }
  checkmate::assert_logical(replace, len = 1L)
  checkmate::assert_logical(append, len = 1L)

  if (isFALSE(replace)) {
    dest_exists <- checkmate::test_file_exists(destination)

    if (dest_exists & isFALSE(append)) stop(
      "'destination' already exists. Please check 'destination' or use ",
      "'replace = TRUE' / append = TRUE in case you want to replace / ",
      "append the existing file."
    )
  }

  # get connection to write to
  # Case: destination is a file path
  if (isFALSE(is_conn)) {
    file.create(destination)
    conn <- file(destination, open = "w", encoding = "UTF-8")
    dest_name <- destination

    # Case: destination is a file connection
  } else {
    dest_name <- "file connection"
  }


  # check and eventually set sink status
  sink_output_active <- sink.number() > 0L
  if (isFALSE(sink_output_active)) {
    sink(file = conn, append = TRUE)
  }

  sink_message_active <- sink.number(type = "message") > 2L
  if (isFALSE(sink_message_active)) {
    sink(file = conn, append = TRUE, type = "message")
  }

  # actually source r_script (and sink everything to 'conn')
  tryCatch(
    source(
      r_script,
      local = local,
      echo = TRUE,
      continue.echo = "+ ",
      skip.echo = 0,
      max.deparse.length = Inf,
      width.cutoff = 80,
      chdir = FALSE
    ),
    # on error, redirect output to console
    error = function(error) {
      suppressWarnings({
        sink(type = "output")
        sink(type = "message")
      })
      warning(
        "An error occured during the execution of '", r_script, "'. ",
        "The log file will be incomplete.",
        call. = FALSE
      )
    }
  )


  if (isFALSE(sink_output_active)) {
    suppressWarnings(sink(type = "output"))
  }
  if (isFALSE(sink_message_active)) {
    suppressWarnings(sink(type = "message"))
  }

  # close connection if it was open within the function call
  if (isFALSE(is_conn)) {
    close(conn)
  }

  if (isFALSE(sink_message_active)) {
    message("Log file for '", r_script, "' written to '", dest_name, "'.")
  }
  return(invisible(NULL))
}
