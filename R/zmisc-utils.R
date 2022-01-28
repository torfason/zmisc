#' Utility function to output an error
#'
#' This function is used to output errors in examples in a tidy manner.
#'
#' @param e The error to wrap
#' @param wrap How many characters before wrapping
#'
#' @return The error is returned invisibly
#'
#' @md
#' @keywords internal
wrap_error <- function(e, wrap=50) {
  cat( paste0("#E> ", strwrap(e$message, width=50), "\n"), sep="" )
  invisible(e)
}

