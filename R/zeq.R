
#' Generate sequence in a safe way
#'
#' @description
#' The [zeq()] function creates an increasing integer sequence, but differs from
#' the standard one in that it will not silently generate a decreasing sequence
#' when the second argument is smaller than the first. If the second argument is
#' one smaller than the first it will generate an empty sequence, if the
#' difference is greater, the function will throw an error.
#'
#' Both arguments must be a single `integerish` value (an `integer`, or a
#' `double` that is very close to one), and neither may be `NA`. Passing a
#' vector of length other than one is an error.
#'
#' @param from The lower bound of the sequence
#' @param to   The higher bound of the sequence
#'
#' @return An `integer` sequence ranging from `from` to `to`, or an empty
#'   `integer` vector if `to` equals `from - 1`.
#'
#' @examples
#' # For increasing sequences, zeq() and seq() are identical
#' zeq(11,15)
#' zeq(11,11)
#'
#' # If second argument equals first-1, an empty sequence is returned
#' zeq(11,10)
#'
#' # If second argument is less than first-1, the function throws an error
#' tryCatch(zeq(11,9), error=wrap_error)
#'
#' # Each bound must be a single whole number, so this errors as well
#' tryCatch(zeq(c(11,12),15), error=wrap_error)
#'
#' @export
zeq <- function(from, to) {

  # Both bounds must be a single, non-NA, integerish value
  chk_znumber(from)
  chk_znumber(to)

  # A decreasing sequence is not allowed here, so error out
  if (to < from - 1) {
    abort(paste0("`to` must not be smaller than `from` - 1 ",
                 "(got from = ", from, ", to = ", to, ")"))
  }

  # seq2() returns an empty integer vector when to == from - 1
  seq2(from, to)
}
