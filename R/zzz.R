#' fndat : Data research and organization for First Nations in Canada
#'
#' @docType package
#' @name fndat
#'
# importFrom

NULL

# ------------------------------------------------------------------------------
# Timestamp
timestamp <- function() format(Sys.time(), format = "%Y-%m-%d")

# ------------------------------------------------------------------------------
# Message
msgInfo <- function(..., appendLF = TRUE) {
  txt <- paste(cli::symbol$info, ...)
  message(crayon::green(txt), appendLF = appendLF)
  invisible(txt)
}
