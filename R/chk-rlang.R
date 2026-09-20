
#' Assertions for arguments and for dots
#'
#' Two assertions for the use cases of `rlang::arg_match()` and `rlang::check_dots_empty()`.
#'
#' - `chk_match()` looks up `x` from `values`, which default to the values in
#'   the caller's own formals, and fails if it is not matched. To assign a
#'   default value to arg, call with `arg <- chk_match(arg)`. If `values` is
#'   not given, `x` must be a symbol, since the values are then read from the
#'   formal of that name.
#' - `chk_dots_empty()` fails if anything was passed through `...`.
#'
#' @param x Value to check and match. A string, or the untouched default of the
#'   argument being matched.
#' @param values Permitted values, as a character vector. `NULL` reads them
#'   from the default of the caller's formal named by `x`, which then has to
#'   be a symbol.
#' @param ... These dots are for future extensions and must be empty.
#' @param multiple Is `x` allowed to hold several values? Each must then match,
#'   and all are returned.
#' @param error_arg Name to report the failure against, in place of the
#'   expression `x` was written as.
#' @return `chk_match()` returns the matched value; `chk_dots_empty()` returns
#'   `NULL` invisibly.
#'
#' @seealso [chk_atomic] and [chk_composite].
#'
#' @rdname chk_other
#' @export
chk_match <- function(x, values = NULL, ..., multiple = FALSE, error_arg = NULL) {

  # Anything in the dots is a typo, not an extension
  if (...length()) chk_dots_empty()

  # If values are not passed, they must be derived from the enclosing formals,
  # and so x must be a symbol
  if (is.null(values)) {
    sym   <- substitute(x)
    env   <- parent.frame()
    frame <- sys.parent()

    # A branch of chk_any() is evaluated in a frame of its own, built from a
    # list, which has no function and no formals.
    if (chk_suppressed(env)) {
      while (chk_suppressed(env)) env <- parent.env(env)
      hit   <- which(vapply(sys.frames(), identical, logical(1L), env))
      frame <- if (length(hit) > 0L) hit[[length(hit)]] else 0L
    }

    if (!is.symbol(sym) || frame == 0L)
      rlang::abort(
        c("`values` must be given unless `x` is an argument of the calling function.",
          i = "Without `values`, they are read from the formal that `x` names.",
          x = if (frame == 0L) "There is no calling function to read them from."
          else paste0("`", deparse1(sym), "` is not a name.")),
        call = rlang::caller_env())
    nm   <- as.character(sym)
    fmls <- formals(sys.function(frame))
    if (!nm %in% names(fmls) || identical(fmls[[nm]], quote(expr = )))
      rlang::abort(
        c("`values` must be given unless `x` is an argument of the calling function.",
          x = paste0("`", nm, "` is not an argument of it with a default."),
          i = "Without `values`, they are read from the default of that argument."),
        call = rlang::caller_env())
    values <- eval(fmls[[nm]], sys.frame(frame))
  }

  # Unmatchable values are a mistake in the call rather than a property of `x`,
  # so they are raised here rather than routed through chk_fail().
  if (!is.character(values) || length(values) == 0L || anyNA(values))
    rlang::abort(c("`values` is not a usable set of values.",
                   x = "It must be a character vector, not empty, and without NA."),
                 call = rlang::caller_env())

  if (!is.character(x)) {
    res <- check_character(x)
  } else {

    # A default that was never overridden arrives whole, and stands for its
    # first value. The comparison is of sets, so that a default listed in
    # another order is still a default.
    if (!multiple && length(x) > 1L && setequal(x, values)) return(x[[1L]])

    bad <- x[!x %in% values]
    if (multiple) {
      if (length(bad) == 0L) return(x)
      res <- match_bullets(bad[[1L]], values)
    } else if (length(x) == 1L && length(bad) == 0L) {
      return(x)
    } else {
      res <- if (length(bad) > 0L) match_bullets(bad[[1L]], values)
      else paste0("Must be length 1, not length ", length(x))
    }
  }

  # One exit for failure, so that the label is settled in one place: chk_fail()
  # reads the expression `x` was written as, unless the caller named it.
  if (is.null(error_arg)) chk_fail(x, res) else chk_fail(x, res, arg = error_arg)
}


#' @rdname chk_other
#' @export
chk_dots_empty <- function() {

  # ...length() is evaluated in the frame that has the dots. Nothing else in
  # this function runs unless that frame has something to report.
  env <- parent.frame()
  n <- eval(quote(...length()), env)
  if (n == 0L) return(invisible(NULL))

  # substitute() reads the dots as written, without forcing them, so an
  # argument that would fail to evaluate is still reported as what it was.
  exprs <- eval(quote(substitute(...())), env)
  nms   <- names(exprs)
  if (is.null(nms)) nms <- character(n)

  # `f(x, )` leaves an extra argument. R ignores it and so does this.
  if (n == 1L && !nzchar(nms[[1L]]) && identical(exprs[[1L]], quote(expr = )))
    return(invisible(NULL))

  labels <- unlist(Map(dots_label, exprs, nms, seq_len(n)), use.names = FALSE)
  res <- c(paste0("Must be empty, but has ", n,
                  if (n == 1L) " argument: " else " arguments: ", toString(labels)),
           if (all(nzchar(nms))) "Are those argument names misspelled?"
           else "Did you forget to name an argument?")

  # `arg` is the dots themselves, and `call` the frame they were passed to,
  # which is the call the misplaced argument was written in.
  chk_fail(NULL, res, arg = "...", call = env)
}

# --- Helpers for chk_dots_empty() error reporting below ---

# The permitted values, spelled out, and a guess when one is close. The guess
# is prefix matching in both directions rather than an edit distance: It
# catches an abbreviation ("c" for "cherry") and a case slip ("Cherry"), but
# does not do more advanced similarity matching.
match_bullets <- function(x, values) {
  quoted <- encodeString(values, quote = "\"")
  one_of <- if (length(quoted) == 1L) {
    quoted
  } else {
    paste0(paste(quoted[-length(quoted)], collapse = ", "),
           if (length(quoted) > 2L) "," else "", " or ", quoted[[length(quoted)]])
  }
  got <- if (is.na(x)) "NA" else encodeString(x, quote = "\"")
  msg <- paste0("Must be one of ", one_of, ", not ", got)
  if (!is.na(x)) {
    lx   <- tolower(x)
    near <- values[startsWith(tolower(values), lx) | startsWith(lx, tolower(values))]
    if (length(near) > 0L)
      msg <- c(msg, paste0("Did you mean ", encodeString(near[[1L]], quote = "\""), "?"))
  }
  msg
}

# One argument of a `...` that should have been empty, as it was written.
dots_label <- function(expr, name, i) {
  written <- deparse1(expr)
  if (nchar(written) > 30L) written <- paste0(substr(written, 1L, 27L), "...")
  paste0(if (nzchar(name)) name else paste0("..", i), " = ", written)
}



