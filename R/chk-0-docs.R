# R/chk-docs.R ----------------------------------------------------------------
#
# Shared documentation blocks for the chk_*() family. The functions themselves
# are defined in chk-generated.R, chk-manual.R and chk-any.R; documentation that
# describes a whole group lives here, so that it is written in one place rather
# than attached to whichever function happens to be defined first.
#
# Functions are documented in one of three groups:
#
#   chk_atomic     scalars and atomic vectors, all generated
#   chk_composite  lists and the objects built out of them
#   chk_other      classes, conditions, and the rlang aliases
#
# The @param tags for chk_atomic are emitted by data-raw/generate-chk.R into
# chk-generated.R, since the union of arguments follows from the spec there.
# The other two topics document their arguments below.


#' Checks for scalars and atomic vectors
#'
#' @description
#' Validity checks for atomic vectors. Each function below takes the same
#' arguments -- `na.ok`, `null.ok`, `attr.ok`, and `length` or `range` where
#' they apply.
#'
#' They are meant to be cheap enough to leave at the top of any function, a
#' passing value with default arguments tends to take under a microsecond on a
#' modern computer; with extra specification arguments it can take up to ten
#' microseconds. Assembling a good message happens on the failing path, which
#' runs once and then stops.
#'
#' | **R Type**    | **Scalar**         | **Vector**           |
#' | ------------- | ------------------ | -------------------- |
#' | Any type      | `chk_scalar(x)`    | `chk_atomic(x)`      |
#' | `logical`     | `chk_flag(x)`      | `chk_logical(x)`     |
#' | `character`   | `chk_string(x)`    | `chk_character(x)`   |
#' | `numeric`     | `chk_number(x)`    | `chk_numeric(x)`     |
#' | `integer`     | `chk_inumber(x)`⁴  | `chk_integer(x)`     |
#' | `double`      | `chk_dnumber(x)`⁴  | `chk_double(x)`      |
#' | `integerish`¹ | `chk_znumber(x)`   | `chk_integerish(x)`  |
#' | `naturalish`² | `chk_count(x)`     | `chk_naturalish(x)`  |
#' | `factor`      | ³                  | `chk_factor(x)`      |
#' | `complex`     | ³                  | `chk_complex(x)`     |
#' | `raw`         | ³                  | `chk_raw(x)`         |
#' | `Date`        | `chk_day(x)`       | `chk_date(x)`        |
#' | `POSIXct`     | `chk_instant(x)`   | `chk_posixct(x)`     |
#'
#' - ¹ `integerish` refers to functional integers (numbers that are very close
#'   to integer values), regardless of type (`integer` or `double`)
#' - ² `naturalish` refers to functional integers restricted to the natural
#'   numbers (zero and positive numbers)
#' - ³ No check functions are provided for scalar `factor`, `complex`, or `raw`
#'
#' @return The original object if the check passes.
#'
#' @seealso [chk_composite] for lists and list-based objects, and [chk_other]
#'   for classes, conditions, and various other checks
#'
#' @examples
#' # A check returns its input (invisibly), so it composes in a pipe
#' c(2, 4, 6) |> chk_numeric(length = 3) |> sum()
#'
#' # One parameter set across the whole family
#' chk_string("abc", range = c(1, 3))
#' chk_integer(1:5, length = c(1, 10))
#'
#' # On failure, the error names the argument as the caller wrote it
#' my_mean <- function(x) {
#'   chk_numeric(x)
#'   sum(x) / length(x)
#' }
#' tryCatch(my_mean("seven"), error = wrap_error)
#'
#' @name chk_atomic
NULL


#' Checks for lists and composite objects
#'
#' @description
#' Checks for composite objects. See [chk_atomic] for scalar and vector types,
#' and [chk_other] for various other checks.
#'
#' | **Function**         | **Passes when**                          |
#' | -------------------- | ---------------------------------------- |
#' | `chk_environment(x)` | `x` is an environment                    |
#' | `chk_list(x)`        | `x` is a list, and carries no class      |
#' | `chk_data_frame(x)`  | `x` is a `data.frame` of sound structure |
#' | `chk_data_table(x)`  | `x` is also a `data.table`               |
#' | `chk_tibble(x)`      | `x` is also a `tbl_df`                   |
#'
#' @param x Object to check.
#' @param ... These dots are for future extensions and must be empty.
#' @param null.ok Is `NULL` permitted?
#' @param length Permitted length. `NULL` for any length, a scalar for one
#'   exact length, or a vector whose first and last elements give the minimum
#'   and the maximum. Neither may be negative, and `NA` at an end, or `Inf` as
#'   the maximum, means no bound there.
#' @param attr.ok Which attributes `x` may carry beyond those intrinsic to its
#'   type: a character vector of permitted attribute names, `FALSE` for none at
#'   all, or `TRUE` for any. Applies to `chk_list()`.
#' @param contains Character vector of names that must be bound in the
#'   environment. Applies to `chk_environment()`.
#' @return The original object if the check passes.
#'
#' @seealso [chk_atomic] for the scalar and vector types, and [chk_other] for
#'   classes, conditions, and the [rlang] aliases.
#'
#' @examples
#' chk_data_frame(mtcars)
#' chk_list(list(a = 1, b = 2), length = 2)
#'
#' # chk_list() only accepts bare (unclassed) lists
#' tryCatch(chk_list(mtcars), error = wrap_error)
#'
#' @name chk_composite
NULL


#' Various other check functions
#'
#' @description
#' Various checks not directly related to atomic vectors (see [chk_atomic]), or
#' composite objects (see [chk_composite]).
#'
#' | **Function**             | **Passes when**                             |
#' | ------------------------ | ------------------------------------------- |
#' | `chk_true(x)`            | `x` is `TRUE` (implement arbitrary checks)  |
#' | `chk_that(x, expr)`      | `expr`, with `.` bound to `x`, is `TRUE`    |
#' | `chk_class(x, classes)`  | `x` inherits from every class in `classes`  |
#' | `chk_match(x, values)`   | `x` matches one of `values`                 |
#' | `chk_dots_empty()`       | nothing was passed through `...`            |
#' | `chk_any(...)`           | at least one of the checks given passes     |
#'
#' `chk_true()` is a catch-all function that can be used to implement arbitrary
#' checks (by checking any expression that should be true).
#'
#' `chk_that()` provides an alternative for checking that an expression is true,
#' but separates the value to be checked (`x`) from the expression evaluated on
#' it (`expr`), which makes an arbitrary condition usable on an object passing
#' through a pipe.
#'
#' `chk_class()` provides a quick way to check the class of an object.
#'
#' `chk_match()` can be used either as an equivalent to [rlang::arg_match()] or
#' [match.arg()] for checking a function argument against default values in a
#' function, or to check that any (`character`) variable x is an element of a
#' (`character`) vector of `values`. When used to select default value in a
#' function, it must be called as `arg <- chk_match(arg)`.
#'
#' `chk_dots_empty()` verifies that no arguments were passed to the `...`
#' parameters, similarly to [rlang::check_dots_empty()].
#'
#' `chk_any(...)` can be used to combine multiple other checks, and will fail
#' only if all constituent checks fail.
#'
#' @param x Object to check.
#' @param ... For `chk_any()`, the checks to try (see examples). For every other
#'   function here the dots must be empty.
#' @param classes Character vector of class names `x` must inherit from.
#' @param ordered Must `classes` appear in that order at the head of `class(x)`?
#' @param expr Expression to evaluate on `x`, which is bound to `.` unless
#'   `bindings` says otherwise.
#' @param bindings Character vector with name (or names) to bind `x` to when
#'   evaluating  `expr`.
#' @param na.ok Are missing values permitted?
#' @param null.ok Is `NULL` permitted?
#' @param values Character vector of values which the object checked by
#'   `chk_match()` must be an element of.
#' @param multiple Logical determining if the return value of `chk_match()` can
#'   contain more than one element.
#' @return The original object if the check passes. `chk_match()` returns the
#'   matched value visibly, `chk_dots_empty()` returns `NULL` invisibly, and
#'   `chk_any()` returns the value of the first argument that passes, which is
#'   the object that was checked.
#'
#' @details
#' `chk_any()` evaluates its arguments in turn and returns the value of
#' the first that passes. If none pass, it raises one error reporting every
#' failure. It is how a composite requirement is written, where each individual
#' `chk_*()` function states only one thing:
#'
#' ```
#' chk_any(chk_string(x), chk_number(x))
#' ```
#'
#' Only the checks `chk_any()` calls itself are candidates. One reached through
#' a helper function, or from inside a lambda passed to `lapply()`, throws where
#' it stands, and so does everything that is not a failed check: a misspelled
#' function, an argument that does not exist, an object that was never bound.
#'
#' The arguments are captured as expressions and evaluated in the calling
#' environment, which rules out two ways of reaching `chk_any()` indirectly.
#' `...` cannot be forwarded into it from another function, and an object cannot
#' be piped into it. Both raise an error rather than being accommodated, since
#' the first would evaluate the checks in the wrong scope and the second would
#' return the piped object as a branch that passed. Write the checks at the call
#' site, naming the object in each.
#'
#'
#' @seealso [chk_atomic] for the scalar and vector types, and [chk_composite]
#'   for lists and composite objects.
#'
#' @examples
#' # Any property that can be written as a condition
#' chk_true(nrow(mtcars) > 10)
#' mtcars |> chk_that(nrow(.) > 10) |> ncol()
#'
#' # Match an argument against the values in its own default
#' plot_kind <- function(kind = c("scatter", "line", "bar")) chk_match(kind)
#' plot_kind()
#' tryCatch(plot_kind("pie"), error = wrap_error)
#'
#' # chk_any() takes the first check that passes
#' x <- "a"
#' chk_any(chk_string(x), chk_number(x))
#' tryCatch(chk_any(chk_number(x), chk_logical(x)), error = wrap_error)
#'
#' @name chk_other
NULL
