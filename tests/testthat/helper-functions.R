clean_cli_output <- function(x) {
    x <- capture_messages(print(x))
    x <- cli::ansi_strip(x)
    x <- x[!(x %in% c("\n", "\r\r", "\r \r"))]
    x <- x[grep("^(──|--)", x, invert = TRUE)]
    gsub("\\✔|\\✓", "v", x)
}
