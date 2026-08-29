#' Generate imports for all assertions functions
#'
#' Generates roxygen2 `@importFrom` statements for all assertion functions.
#'
#' @param prefix Prefix used to select exported names.
#' @param width Maximum output line width.
#'
#' @return A character string containing `@importFrom` statements and `NULL`.
#' @keywords internal
#' @export
import_all_chk <- function(prefix = "chk_", width = 80) {
  cat(import_all_with_prefix(prefix, width))
}

# Helper for generating the import
import_all_with_prefix <- function(prefix, width = 80) {

  # Determine which package and functions we are dealing with
  pkg      <- getNamespaceName(environment(sys.function()))
  funs_all <- getNamespaceExports(pkg)
  funs_pfx <- sort(funs_all[startsWith(funs_all, prefix)])

  # Stop with informative error if no functions match
  stopifnot(length(funs_pfx) > 0)

  # Generate the output string
  header <- paste("#' @importFrom", pkg)
  paste0(
    paste(header, strwrap(paste(funs_pfx, collapse = " "),
                          width = width - nchar(header) - 1
    ), collapse = "\n"),
    "\nNULL"
  )
}
