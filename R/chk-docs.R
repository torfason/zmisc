# R/chk-docs.R ----------------------------------------------------------------
#
# Shared documentation blocks for the chk_*() family. The functions themselves
# are defined in chk-generated.R, chk-manual.R and chk-any.R; the prose that
# describes a whole group lives here, so that it is written in one place rather
# than attached to whichever function happens to be defined first.

#' Assertion functions for scalars and atomic vectors
#'
#' @description
#' The [checkmate] type checks, adapted to raise [rlang] style errors. Every
#' function below takes the same arguments -- `na.ok`, `null.ok`, `attr.ok`,
#' and `length` or `range` where they apply -- rather than the argument set of
#' the [checkmate] function behind it. The dots are reserved, so a name that
#' does not match raises rather than being quietly ignored.
#'
#' They are meant to be cheap enough to leave at the top of any function: the
#' passing case is one call to the backing `check_*()`, a test, and a return.
#' Assembling a good message happens on the failing path, which runs once and
#' then stops.
#'
#' | **R Type**    | **Scalar**         | **Vector**           |
#' | ------------- | ------------------ | -------------------- |
#' | `logical`     | `chk_flag(x)`      | `chk_logical(x)`     |
#' | `character`   | `chk_string(x)`    | `chk_character(x)`   |
#' | `numeric`     | `chk_number(x)`    | `chk_numeric(x)`     |
#' | `integer`     | `chk_inumber(x)`⁴  | `chk_integer(x)`     |
#' | `double`      | `chk_dnumber(x)`⁴  | `chk_double(x)`      |
#' | `integerish`¹ | `chk_znumber(x)`   | `chk_integerish(x)`  |
#' | `naturalish`² | `chk_count(x)`     | `chk_naturalish(x)`⁴ |
#' | `factor`      | ³                  | `chk_factor(x)`      |
#' | `complex`     | ³                  | `chk_complex(x)`     |
#' | `raw`         | ³                  | `chk_raw(x)`         |
#' | `Date`        | `chk_day(x)`⁴      | `chk_date(x)`        |
#' | `POSIXct`     | `chk_instant(x)`⁴  | `chk_posixct(x)`     |
#' | Any type      | `chk_scalar(x)`    | `chk_atomic(x)`⁵     |
#'
#'
#' - ¹ `integerish` refers to functional integers (numbers that are very close
#'   to integer values), regardless of type (`integer` or `double` )
#' - ² `naturalish` refers to functional integers restricted to the natural
#'   numbers (zero and positive numbers)
#' - ³ No assertion functions are provided for scalar `factor`, `complex`, or `raw`
#' - ⁴ Not available in the [checkmate] package
#' - ⁵ Note that [checkmate::assert_vector()] accepts either a `vector` or a
#'   `list`, which is seldom what is wanted and is therefore *not* implemented
#'   here.
#'
#' @return The original object if the assertion passes.
#'
#' @seealso [checkmate_rlang_other] for containers, classes, and arbitrary
#'   conditions.
#'
#' @name checkmate_rlang
NULL
