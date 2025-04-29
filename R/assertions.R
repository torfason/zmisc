
#' @importFrom checkmate check_flag check_string check_number check_int
#'   check_count check_list check_class check_choice check_integer check_double
#'   check_numeric check_logical check_character check_raw check_date
#'   check_integerish check_complex check_factor qtest test_integer
NULL

#' @importFrom rlang arg_match seq2 abort
#' @importFrom rlang .data
NULL

test_inumber_perhaps_faster_but_untested <- function(x, na.ok = FALSE, null.ok = FALSE, lower = -Inf, upper = Inf) {
  if (is.null(x)) return(null.ok)
  if (!is.integer(x)) return(FALSE)
  if (length(x) != 1L) return(FALSE)
  if (is.na(x)) return(na.ok)
  if (x < lower || x > upper) return(FALSE)
  TRUE
}


# Check for integer number (scalar integer)
check_inumber <- function(x, na.ok = FALSE, lower = -Inf, upper = Inf, null.ok = FALSE) {
  if (!isTRUE(check_integer(x, len = 1, any.missing = na.ok, lower = lower, upper = upper, null.ok = null.ok))) {
    result <- check_integer(x, len = 1, any.missing = na.ok, lower = lower, upper = upper, null.ok = null.ok)
    if (result == "Contains missing values (element 1)") {
      return("May not be NA")
    }  else {
      return(result)
    }
  }
  TRUE
}

# Check for double number (scalar double)
check_dnumber <- function(x, na.ok = FALSE, lower = -Inf, upper = Inf, finite = FALSE, null.ok = FALSE) {
  if (!isTRUE(check_double(x, len = 1, any.missing = na.ok, lower = lower, upper = upper, finite = finite, null.ok = null.ok))) {
    result <- check_double(x, len = 1, any.missing = na.ok, lower = lower, upper = upper, finite = finite, null.ok = null.ok)
    if (result == "Contains missing values (element 1)") {
      return("May not be NA")
    }  else {
      return(result)
    }
  }
  TRUE
}

# Check for naturalish numerics (x integerish & x >= 0 (1 if positive == TRUE))
check_naturalish <- function (x, tol = sqrt(.Machine$double.eps), positive = FALSE,
                              any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL,
                              max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL,
                              typed.missing = FALSE, null.ok = FALSE) {
  result <- check_integerish(x, tol = tol, lower = ifelse(positive, 1, 0), upper = Inf,
                     any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len,
                     max.len = max.len, unique = unique, sorted = sorted, names = names,
                     typed.missing = typed.missing, null.ok = null.ok)
  ifelse(isTRUE(result), result, sub(x = result, "integerish", "naturalish"))
}

# Check for single day (scalar Date)
check_day <- function(x, na.ok = FALSE, lower = -Inf, upper = Inf, null.ok = FALSE) {
  if (!isTRUE(check_date(x, len = 1, any.missing = na.ok, lower = lower, upper = upper, null.ok = null.ok))) {
    result <- check_date(x, len = 1, any.missing = na.ok, lower = lower, upper = upper, null.ok = null.ok)
    if (is.null(x) && !null.ok) {
      return("Must be of type 'day', not 'NULL'")
    } else  if (!is.null(x) && length(x) ==1 && all(is.na(x)) && !na.ok) {
      return("May not be NA")
    }  else {
      return(result)
    }
  }
  TRUE
}


#' Assert that no dots arguments are passed
#'
#' `assert_dots_empty()` is an alias for [rlang::check_dots_empty()], provided
#' for naming consistency with other assertion functions. It throws an error if
#' any arguments are passed through `...`.
#'
#' @inherit rlang::check_dots_empty description return
#' @inheritParams rlang::check_dots_empty
#' @rdname checkmate_rlang_dots
#' @export
assert_dots_empty <- rlang::check_dots_empty



#' Assertion functions adapted for rlang output
#'
#' @description
#' Most common [checkmate] functions, adapted to output [rlang] style error
#' messages on failed assertions. The actual checking is done by
#' [checkmate::qtest()], [checkmate::check_flag()] and related functions.
#'
#' | **R Type**    | **Scalar**           | **Vector**              |
#' | ------------- | -------------------- | ----------------------- |
#' | `logical`     | `assert_flag(x)`     | `assert_logical(x)`     |
#' | `character`   | `assert_string(x)`   | `assert_character(x)`   |
#' | `numeric`     | `assert_number(x)`   | `assert_numeric(x)`     |
#' | `integer`     | `assert_inumber(x)`⁴ | `assert_integer(x)`     |
#' | `double`      | `assert_dnumber(x)`⁴ | `assert_double(x)`      |
#' | `integerish`¹ | `assert_int(x)`      | `assert_integerish(x)`  |
#' | `naturalish`² | `assert_count(x)`    | `assert_naturalish(x)`⁴ |
#' | `factor`      | ³                    | `assert_factor(x)`      |
#' | `complex`     | ³                    | `assert_complex(x)`     |
#' | `raw`         | ³                    | `assert_raw(x)`         |
#' | `Date`        | `assert_day(x)`      | `assert_date(x)`        |
#'
#' - ¹ `integerish` refers to functional integers (numbers that are very close
#'   to integer values), regardless of type (`integer` or `double` )
#' - ² `naturalish` refers to functional integers restricted to the natural
#'   numbers (zero and positive numbers
#' - ³ No assertion functions are provided for scalar `factor`, `complex`, or `raw`
#' - ⁴ Not available in the [checkmate] package
#'
#' | a | b | c |
#' |---|---|---|
#' | a | b | c |
#'
#' @param x The variable to assert
#' @param ... Additional parameters passed to corresponding [checkmate]
#'   functions [checkmate::qtest()], [checkmate::check_flag()], etc.
#' @return The original object if the assertion passes.
#'
#' @rdname checkmate_rlang
#' @export
qassert <- function(x, ...) {
  if (!isTRUE(qtest(x, ...)))
    rlang::abort(qtest(x, ...))
  invisible(x)
}

# --- Scalar assertions ----

#'
#' @rdname checkmate_rlang
#' @export
assert_flag <- function(x, ...) {
  if (!isTRUE(check_flag(x, ...)))
    rlang::abort(check_flag(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
assert_string <- function(x, ...) {
  if (!isTRUE(check_string(x, ...)))
    rlang::abort(check_string(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
assert_number <- function(x, ...) {
  if (!isTRUE(check_number(x, ...)))
    rlang::abort(check_number(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
assert_inumber <- function(x, ...) {
  if (!isTRUE(check_inumber(x, ...))) {
    rlang::abort(check_inumber(x, ...))
  }
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
assert_dnumber <- function(x, ...) {
  if (!isTRUE(check_dnumber(x, ...))) {
    result <- check_dnumber(x, ...)
    if (result == "Contains missing values (element 1)")
      result <- "May not be NA"
    rlang::abort(result)
  }
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
assert_int <- function(x, ...) {
  if (!isTRUE(check_int(x, ...)))
    rlang::abort(check_int(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
assert_count <- function(x, ...) {
  if (!isTRUE(check_count(x, ...)))
    rlang::abort(check_count(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
assert_day <- function(x, ...) {
  if (!isTRUE(check_day(x, ...))) {
    result <- check_day(x, ...)
    if (result == "Contains missing values (element 1)")
      result <- "May not be NA"
    rlang::abort(result)
  }
  invisible(x)
}

# ---- Vector assertions ----

#' @rdname checkmate_rlang
#' @export
assert_logical <- function(x, ...) {
  if (!isTRUE(check_logical(x, ...)))
    rlang::abort(check_logical(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
assert_character <- function(x, ...) {
  if (!isTRUE(check_character(x, ...)))
    rlang::abort(check_character(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
assert_numeric <- function(x, ...) {
  if (!isTRUE(check_numeric(x, ...)))
    rlang::abort(check_numeric(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
assert_integer <- function(x, ...) {
  if (!isTRUE(check_integer(x, ...)))
    rlang::abort(check_integer(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
assert_double <- function(x, ...) {
  if (!isTRUE(check_double(x, ...)))
    rlang::abort(check_double(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
assert_integerish <- function(x, ...) {
  if (!isTRUE(check_integerish(x, ...)))
    rlang::abort(check_integerish(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
assert_naturalish <- function(x, ...) {
  if (!isTRUE(check_naturalish(x, ...)))
    rlang::abort(check_naturalish(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
assert_factor <- function(x, ...) {
  if (!isTRUE(check_factor(x, ...)))
    rlang::abort(check_factor(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
assert_complex <- function(x, ...) {
  if (!isTRUE(check_complex(x, ...)))
    rlang::abort(check_complex(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
assert_raw <- function(x, ...) {
  if (!isTRUE(check_raw(x, ...)))
    rlang::abort(check_raw(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
assert_date <- function(x, ...) {
  if (!isTRUE(check_date(x, ...)))
    rlang::abort(check_date(x, ...))
  invisible(x)
}

