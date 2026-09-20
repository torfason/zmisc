# Assertions that are not generated.
#
# These follow the shape of chk-generated.R -- reserved dots, one call to the
# backing check_*(), invisible(x) on success, chk_fail() on failure -- but not
# its parameter set. Containers take no attr.ok argument, because the policy
# exists to catch labelled vectors and matrices posing as bare vectors and
# there is no container analogue; a data.frame's class and row.names are not
# extras, and a grouped tibble's `groups` is not a defect. chk_list() is the
# exception, and applies the "names" policy itself rather than offering it as
# an argument: a bare list is a vector, checkmate::check_list() passes anything
# of type list including data frames, and the absence of a class attribute is
# the only thing separating the two. Containers get no `range`, and `length`
# only where it counts something a caller would recognize. Everything else
# checkmate offers stays pinned, so a misspelled argument raises rather than
# being silently dropped.

#### CONTAINERS ####

# chk_environment(): container, backed by check_environment()

#' @rdname chk_composite
#' @export
chk_environment <- function(x, ..., null.ok = FALSE, contains = character()) {

  # No arguments, return on fastest path
  if (nargs() == 1L && isTRUE(check_environment(x)))
      return(invisible(x))

  # Anything in the dots is a typo, not an extension
  chk_dots_empty()

  res <- check_environment(x, null.ok = null.ok, contains = contains)
  if (isTRUE(res)) return(invisible(x))
  chk_fail(x, res)
}

# chk_list(): container, backed by check_list()
# pinned: types = character(0L), any.missing = TRUE, all.missing = TRUE,
#   unique = FALSE, names = NULL
# Allowed attributes generally follow the same policy as for atomic types, with
# the same "names" default, which is exactly is.vector(x, "list") and is what
# the fast path inlines. Arbitrary classed lists pass on attr.ok = "class", or
# TRUE for any.
# NOTE: A data.frame never passes this check, regardless of the attr.ok value:
# that is checkmate's own rule, since check_list() reports it as a type of its
# own rather than as a list.

#' @rdname chk_composite
#' @export
chk_list <- function(x, ..., null.ok = FALSE, attr.ok = "names", length = NULL) {

  # No arguments, return on fastest path
  if (nargs() == 1L && isTRUE(check_list(x)) && is.vector(x, "list"))
    return(invisible(x))

  # Anything in the dots is a typo, not an extension
  chk_dots_empty()

  len <- lo_hi_count(length)
  res <- check_list(x,
                    null.ok = null.ok,
                    len = len$exact,
                    min.len = len$min,
                    max.len = len$max)
  if (isTRUE(res) && attrs_ok(x, attr.ok)) return(invisible(x))
  chk_fail(x, res, attr.ok)
}

# chk_data_frame(): container, backed by check_data_frame()
# pinned: types = character(0L), any.missing = TRUE, all.missing = TRUE,
#   min.rows = NULL, max.rows = NULL, min.cols = NULL, max.cols = NULL,
#   nrows = NULL, ncols = NULL, row.names = NULL, col.names = NULL

#' @rdname chk_composite
#' @export
chk_data_frame <- function(x, ..., null.ok = FALSE) {

  # No arguments, return on fastest path
  if (nargs() == 1L && isTRUE(check_data_frame(x)))
      return(invisible(x))

  # Anything in the dots is a typo, not an extension
  chk_dots_empty()

  res <- check_data_frame(x, null.ok = null.ok)
  if (isTRUE(res)) return(invisible(x))
  chk_fail(x, res)
}

# chk_data_table(): container, backed by check_data_table()
# pinned: key = NULL, index = NULL, types = character(0L), any.missing = TRUE,
#   all.missing = TRUE, min.rows = NULL, max.rows = NULL, min.cols = NULL,
#   max.cols = NULL, nrows = NULL, ncols = NULL, row.names = NULL,
#   col.names = NULL

#' @rdname chk_composite
#' @export
chk_data_table <- function(x, ..., null.ok = FALSE) {

  # No arguments, return on fastest path
  if (nargs() == 1L && isTRUE(check_data_table(x)))
      return(invisible(x))

  # Anything in the dots is a typo, not an extension
  chk_dots_empty()

  res <- check_data_table(x, null.ok = null.ok)
  if (isTRUE(res)) return(invisible(x))
  chk_fail(x, res)
}

# chk_tibble(): container, backed by check_tibble()
# pinned: types = character(0L), any.missing = TRUE, all.missing = TRUE,
#   min.rows = NULL, max.rows = NULL, min.cols = NULL, max.cols = NULL,
#   nrows = NULL, ncols = NULL, row.names = NULL, col.names = NULL

#' @rdname chk_composite
#' @export
chk_tibble <- function(x, ..., null.ok = FALSE) {

  # No arguments, return on fastest path
  if (nargs() == 1L && isTRUE(check_tibble(x)))
      return(invisible(x))

  # Anything in the dots is a typo, not an extension
  chk_dots_empty()

  res <- check_tibble(x, null.ok = null.ok)
  if (isTRUE(res)) return(invisible(x))
  chk_fail(x, res)
}

#### OTHER ####

# chk_class(): backed by check_class()
# The fast path needs two arguments, since `classes` is required.

#' @rdname chk_other
#' @export
chk_class <- function(x, classes, ..., null.ok = FALSE, ordered = FALSE) {

  # No optional arguments, return on fastest path
  if (nargs() == 2L && isTRUE(check_class(x, classes)))
      return(invisible(x))

  # Anything in the dots is a typo, not an extension
  chk_dots_empty()

  res <- check_class(x, classes, ordered = ordered, null.ok = null.ok)
  if (isTRUE(res)) return(invisible(x))
  chk_fail(x, res)
}

# chk_true(): backed by check_true()
# The catch-all: any property of any object, expressed as a condition.

#' @rdname chk_other
#' @export
chk_true <- function(x, ..., na.ok = FALSE) {

  # No arguments, return on fastest path
  if (nargs() == 1L && isTRUE(check_true(x)))
      return(invisible(x))

  # Anything in the dots is a typo, not an extension
  chk_dots_empty()

  res <- check_true(x, na.ok = na.ok)
  if (isTRUE(res)) return(invisible(x))
  chk_fail(x, res)
}

# chk_that(): backed by check_true()
# chk_true() variant that works in pipe by passing var and expr separately

#' @rdname chk_other
#' @export
chk_that <- function(x, expr, ..., na.ok = FALSE, .varnames = ".") {

  # No optional arguments, evaluate against `.` alone
  if (nargs() == 2L) {
    value <- eval(substitute(expr), list(. = x), parent.frame())
    if (isTRUE(value)) return(invisible(x))
  } else {
    if (...length()) chk_dots_empty()
    chk_character(.varnames)
    bindings <- rep(list(x), length(.varnames))
    names(bindings) <- .varnames
    value <- eval(substitute(expr), bindings, parent.frame())
  }

  res <- check_true(value, na.ok = na.ok)
  if (isTRUE(res)) return(invisible(x))
  chk_fail(x, res, attr.ok = TRUE, arg = deparse1(substitute(expr)))
}
