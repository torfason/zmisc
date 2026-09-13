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
# only where it counts something a caller would recognise. Everything else
# checkmate offers stays pinned, so a misspelled argument raises rather than
# being silently dropped.

#' Assertion functions for objects that are not atomic vectors
#'
#' @description
#' Assertions for containers and for properties that no type check covers,
#' with [rlang] style error messages. See [checkmate_rlang] for the scalar and
#' vector types.
#'
#' | **Function**            | **Passes when**                                 |
#' | ----------------------- | ----------------------------------------------- |
#' | `chk_environment(x)`    | `x` is an environment                           |
#' | `chk_list(x)`           | `x` is a list, and carries no class             |
#' | `chk_data_frame(x)`     | `x` is a `data.frame` of sound structure        |
#' | `chk_data_table(x)`     | `x` is also a `data.table`                      |
#' | `chk_tibble(x)`         | `x` is also a `tbl_df`                          |
#' | `chk_class(x, classes)` | `x` inherits from every class in `classes`      |
#' | `chk_true(x)`           | `x` is `TRUE`                                   |
#' | `chk_that(x, expr)`     | `x` mapped to `.` results in `expr` being TRUE  |
#'
#' `chk_true()` is the catch-all: any property of any object that can be
#' written as a condition, at the cost of a message that can only report that
#' the condition was not met.
#'
#' `chk_that()` is a variant of `chk_true()` that separates the value to be
#' checked (`x`) from the expression to be evaluated on it (`expr`). This
#' can be helpful when evaluating an arbitrary condition on an object passing
#' through a pipe.
#'
#' These take far fewer arguments than their [checkmate] counterparts. The
#' container checks carry no `attr.ok`, since a `data.frame` is its class and
#' its row names, and only `chk_list()` takes a `length`.
#'
#' @param x Object to check.
#' @param ... These dots are for future extensions and must be empty.
#' @param na.ok Are missing values permitted?
#' @param null.ok Is `NULL` permitted?
#' @param length Permitted length. `NULL` for any length, a scalar for one
#'   exact length, or a vector whose first and last elements give the minimum
#'   and the maximum. Neither may be negative, and `NA` at an end, or `Inf` as
#'   the maximum, means no bound there.
#' @param classes Character vector of class names `x` must inherit from.
#' @param contains Character vector of names that must be bound in the
#'   environment.
#' @param ordered Must `classes` appear in that order at the head of
#'   `class(x)`?
#' @return The original object if the assertion passes.
#'
#' @seealso [checkmate_rlang] for the scalar and vector types.
#'
#' @name checkmate_rlang_other
NULL

#### CONTAINERS ####

# chk_environment(): container, backed by check_environment()

#' @rdname checkmate_rlang_other
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
# check_list() passes anything of type list, so the attribute policy is what
# keeps a data.frame out. Fixed at "names", not offered as an argument.

#' @rdname checkmate_rlang_other
#' @export
chk_list <- function(x, ..., null.ok = FALSE, length = NULL) {

  # No arguments, return on fastest path
  if (nargs() == 1L && isTRUE(check_list(x)) && is.vector(x, "any"))
      return(invisible(x))

  # Anything in the dots is a typo, not an extension
  chk_dots_empty()

  len <- lo_hi_count(length)
  res <- check_list(x,
                    null.ok = null.ok,
                    len = len$exact,
                    min.len = len$min,
                    max.len = len$max)
  if (isTRUE(res) && attrs_ok(x, "names")) return(invisible(x))
  chk_fail(x, res, "names")
}

# chk_data_frame(): container, backed by check_data_frame()
# pinned: types = character(0L), any.missing = TRUE, all.missing = TRUE,
#   min.rows = NULL, max.rows = NULL, min.cols = NULL, max.cols = NULL,
#   nrows = NULL, ncols = NULL, row.names = NULL, col.names = NULL

#' @rdname checkmate_rlang_other
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

#' @rdname checkmate_rlang_other
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

#' @rdname checkmate_rlang_other
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

#' @rdname checkmate_rlang_other
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

#' @rdname checkmate_rlang_other
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

#' @rdname checkmate_rlang_other
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
