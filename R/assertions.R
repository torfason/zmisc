
#' @importFrom checkmate  check_flag check_string check_number check_int
#' @importFrom checkmate  check_count check_class check_choice check_integer check_double
#' @importFrom checkmate  check_numeric check_logical check_character check_raw check_date
#' @importFrom checkmate  check_integerish check_complex check_factor qtest
#' @importFrom checkmate  check_list check_data_frame check_data_table check_tibble
#' @importFrom checkmate  check_scalar check_atomic check_environment check_posixct
NULL

# arg_match is not used yet, but an upcoming assertion will rely on it
#' @importFrom rlang arg_match seq2 abort
NULL

# A hand-rolled test_inumber(), avoiding the round trip through checkmate.
# Kept for reference as a possible fast path, but neither used nor tested.
#
# test_inumber <- function(x, na.ok = FALSE, null.ok = FALSE, lower = -Inf, upper = Inf) {
#   if (is.null(x)) return(null.ok)
#   if (!is.integer(x)) return(FALSE)
#   if (length(x) != 1L) return(FALSE)
#   if (is.na(x)) return(na.ok)
#   if (x < lower || x > upper) return(FALSE)
#   TRUE
# }


# Check for integer number (scalar integer)
old_check_inumber <- function(x, na.ok = FALSE, lower = -Inf, upper = Inf, null.ok = FALSE) {
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
old_check_dnumber <- function(x, na.ok = FALSE, lower = -Inf, upper = Inf, finite = FALSE, null.ok = FALSE) {
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
old_check_naturalish <- function (x, tol = sqrt(.Machine$double.eps), positive = FALSE,
                              any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL,
                              max.len = NULL, unique = FALSE, sorted = FALSE, names = NULL,
                              typed.missing = FALSE, null.ok = FALSE) {
  result <- check_integerish(x, tol = tol, lower = ifelse(positive, 1, 0), upper = Inf,
                     any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len,
                     max.len = max.len, unique = unique, sorted = sorted, names = names,
                     typed.missing = typed.missing, null.ok = null.ok)
  ifelse(isTRUE(result), result, sub(x = result, "integerish", "naturalish"))
}

# Check for single instant (scalar POSIXct)
#
# Mirrors check_day(), except that lower/upper default to NULL rather than
# +-Inf, because check_posixct() insists that any bound it is given is itself
# a single POSIXct time.
check_instant <- function(x, na.ok = FALSE, lower = NULL, upper = NULL, null.ok = FALSE) {
  if (!isTRUE(check_posixct(x, len = 1, any.missing = na.ok, lower = lower, upper = upper, null.ok = null.ok))) {
    result <- check_posixct(x, len = 1, any.missing = na.ok, lower = lower, upper = upper, null.ok = null.ok)
    if (is.null(x) && !null.ok) {
      return("Must be of type 'instant', not 'NULL'")
    } else  if (!is.null(x) && length(x) ==1 && all(is.na(x)) && !na.ok) {
      return("May not be NA")
    }  else {
      return(result)
    }
  }
  TRUE
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
#' `chk_dots_empty()` is an alias for [rlang::check_dots_empty()], provided
#' for naming consistency with other assertion functions. It throws an error if
#' any arguments are passed through `...`.
#'
#' @inherit rlang::check_dots_empty description return
#' @inheritParams rlang::check_dots_empty
#' @rdname checkmate_rlang_dots
#' @export
chk_dots_empty <- rlang::check_dots_empty



#' Assertion functions adapted for rlang output
#'
#' @description
#' Most common [checkmate] functions, adapted to output [rlang] style error
#' messages on failed assertions. The actual checking is done by
#' [checkmate::qtest()], [checkmate::check_flag()] and related functions.
#'
#' ### Performance
#'
#' These functions are meant to be cheap enough to leave in place at the top of
#' any function, so the passing case is kept to the smallest amount of work
#' that will do: a single call to the underlying `check_*()` function, a test of
#' the result, and a return. Anything more expensive belongs on the failing
#' path, which runs once and then stops, and where the cost of assembling a
#' better message does not matter.
#'
#' ### Scalars and (atomic) vectors
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
#' ### Composite Objects
#'
#' | **R Type**          | **Function**         | **Note**                                                     |
#' | ------------------- | -------------------- | ------------------------------------------------------------ |
#' | `environment`       | `chk_environment(x)` | `is.environment(x)`                                          |
#' | `list`              | `chk_list(x)`        | `is.list(x)` *and* x is unclassed.                           |
#' | `data.frame`        | `chk_data_frame(x)`  | `is.list(x)`, with class `data.frame` and correct structure. |
#' | `data.table`        | `chk_data_table(x)`⁴ | `data.table::is.data.table(x)` *and* x is a `data.frame`.    |
#' | `tibble` (`tbl_df`) | `chk_tibble(x)`      | `tibble::is_tibble(x)` *and* x is a `data.frame`.            |
#'
#' @param x The variable to assert
#' @param ... Additional parameters passed to corresponding [checkmate]
#'   functions [checkmate::qtest()], [checkmate::check_flag()], etc.
#' @return The original object if the assertion passes.
#'
#' @rdname checkmate_rlang
#' @export
qassert <- function(x, ...) {
  if (!qtest(x, ...))
    rlang::abort(qassert_message(x, ...))
  invisible(x)
}

# qtest() reports only TRUE/FALSE, so the message has to be recovered from
# checkmate::qassert(). Only ever reached once the assertion has failed.
qassert_message <- function(x, ...) {
  conditionMessage(tryCatch(checkmate::qassert(x, ...), error = identity))
}

#### SCALAR AND VECTOR ASSERTIONS ####

# --- Scalar assertions ----

#'
#' @rdname checkmate_rlang
#' @export
chk_flag <- function(x, ...) {
  if (!isTRUE(check_flag(x, ...)))
    rlang::abort(check_flag(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
chk_string <- function(x, ...) {
  if (!isTRUE(check_string(x, ...)))
    rlang::abort(check_string(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
chk_number <- function(x, ...) {
  if (!isTRUE(check_number(x, ...)))
    rlang::abort(check_number(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
chk_inumber <- function(x, ...) {
  if (!isTRUE(check_inumber(x, ...)))
    rlang::abort(check_inumber(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
chk_dnumber <- function(x, ...) {
  if (!isTRUE(check_dnumber(x, ...)))
    rlang::abort(check_dnumber(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
chk_znumber <- function(x, ...) {
  if (!isTRUE(check_int(x, ...)))
    rlang::abort(check_int(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
chk_count <- function(x, ...) {
  if (!isTRUE(check_count(x, ...)))
    rlang::abort(check_count(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
chk_day <- function(x, ...) {
  if (!isTRUE(check_day(x, ...)))
    rlang::abort(check_day(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
chk_instant <- function(x, ...) {
  if (!isTRUE(check_instant(x, ...)))
    rlang::abort(check_instant(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
chk_scalar <- function(x, ...) {
  if (!isTRUE(check_scalar(x, ...)))
    rlang::abort(check_scalar(x, ...))
  invisible(x)
}

# ---- Vector assertions ----

#' @rdname checkmate_rlang
#' @export
chk_logical <- function(x, ...) {
  if (!isTRUE(check_logical(x, ...)))
    rlang::abort(check_logical(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
chk_character <- function(x, ...) {
  if (!isTRUE(check_character(x, ...)))
    rlang::abort(check_character(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
chk_numeric <- function(x, ...) {
  if (!isTRUE(check_numeric(x, ...)))
    rlang::abort(check_numeric(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
chk_integer <- function(x, ...) {
  if (!isTRUE(check_integer(x, ...)))
    rlang::abort(check_integer(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
chk_double <- function(x, ...) {
  if (!isTRUE(check_double(x, ...)))
    rlang::abort(check_double(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
chk_integerish <- function(x, ...) {
  if (!isTRUE(check_integerish(x, ...)))
    rlang::abort(check_integerish(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
chk_naturalish <- function(x, ...) {
  if (!isTRUE(check_naturalish(x, ...)))
    rlang::abort(check_naturalish(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
chk_factor <- function(x, ...) {
  if (!isTRUE(check_factor(x, ...)))
    rlang::abort(check_factor(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
chk_complex <- function(x, ...) {
  if (!isTRUE(check_complex(x, ...)))
    rlang::abort(check_complex(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
chk_raw <- function(x, ...) {
  if (!isTRUE(check_raw(x, ...)))
    rlang::abort(check_raw(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
chk_date <- function(x, ...) {
  if (!isTRUE(check_date(x, ...)))
    rlang::abort(check_date(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
chk_posixct <- function(x, ...) {
  if (!isTRUE(check_posixct(x, ...)))
    rlang::abort(check_posixct(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
chk_atomic <- function(x, ...) {
  if (!isTRUE(check_atomic(x, ...)))
    rlang::abort(check_atomic(x, ...))
  invisible(x)
}

#### COMPOSITE OBJECTS ####

#' @rdname checkmate_rlang
#' @export
chk_environment <- function(x,  ...) {
  if (!isTRUE(check_environment(x, ...)))
    rlang::abort(check_environment(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
chk_list <- function(x, ...) {
  if (!isTRUE(check_list(x, ...)))
    rlang::abort(check_list(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
chk_data_frame <- function(x, ...) {
  if (!isTRUE(check_data_frame(x, ...)))
    rlang::abort(check_data_frame(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
chk_data_table <- function(x, ...) {
  if (!isTRUE(check_data_table(x, ...)))
    rlang::abort(check_data_table(x, ...))
  invisible(x)
}

#' @rdname checkmate_rlang
#' @export
chk_tibble <- function(x, ...) {
  if (!isTRUE(check_tibble(x, ...)))
    rlang::abort(check_tibble(x, ...))
  invisible(x)
}

#### SPECIAL CASES ####

# --- Type, class, and structure assertions ----


#' @rdname checkmate_rlang
#' @export
chk_class <- function(x, ...) {
  if (!isTRUE(check_class(x, ...)))
    rlang::abort(check_class(x, ...))
  invisible(x)
}

# --- Set and value assertions ----

#' @param choices A vector of values representing the which x must be an
#'   element of.
#'
#' @rdname checkmate_rlang
#' @export
chk_choice <- function(x, choices, ...) {
  if (!isTRUE(check_choice(x, choices, ...)))
    rlang::abort(check_choice(x, choices, ...))
  invisible(x)
}

