# The composite assertion.
#
# chk_any() evaluates its arguments itself rather than letting them arrive as
# forced promises, because it has to run them in an environment it controls:
# the suppression flag described in chk-support.R has to sit in the frame the
# assertion is called from, and a promise is evaluated in the frame that wrote
# it. Each branch gets a frame of its own, built by eval() from the flag list
# and parented on the caller's, so the assertions see exactly what they would
# have seen and nothing one branch assigns is visible to the next.

#' Assert that at least one of several assertions passes
#'
#' @description
#' `chk_any()` evaluates its arguments in turn and returns the value of the
#' first assertion that passes. If none pass, it raises one error reporting
#' every failure. It is how a composite requirement is written, where each
#' `chk_*()` function states only one thing:
#'
#' ```
#' chk_any(chk_string(x), chk_number(x))
#' ```
#'
#' @details
#' Only the assertions `chk_any()` calls itself are candidates. An assertion
#' reached through a helper function, or from inside a lambda passed to
#' `lapply()`, throws where it stands, and so does everything that is not an
#' assertion failure: a misspelled function, an argument that does not exist,
#' an object that was never bound. That is the difference between this and
#' wrapping the branches in [tryCatch()], which cannot tell a failed check from
#' a typo, and it is also why the passing case costs microseconds rather than
#' the milliseconds an [rlang::abort()] spends capturing a backtrace.
#'
#' The arguments are captured as expressions and evaluated in the calling
#' environment, which rules out two ways of reaching `chk_any()` indirectly.
#' `...` cannot be forwarded into it from another function, and an object
#' cannot be piped into it. Both raise an error rather than being accommodated,
#' since the first would evaluate the assertions in the wrong scope and the
#' second would return the piped object as a branch that passed. Write the
#' assertions at the call site, naming the object in each.
#'
#' @param ... Assertion calls. Evaluated left to right, stopping at the first
#'   that passes. Must not be named.
#' @return The value of the first argument that passes, invisibly. This is the
#'   object that was asserted on, so `chk_any()` can be used inline the same way
#'   the individual assertions can.
#'
#' @seealso [checkmate_rlang] for the scalar and vector types,
#'   [checkmate_rlang_other] for containers and `chk_true()`.
#'
#' @examples
#' x <- "a"
#' chk_any(chk_string(x), chk_number(x))
#'
#' y <- 3
#' chk_any(chk_string(y), chk_number(y))
#'
#' @export
chk_any <- function(...) {
  exprs <- as.list(substitute(list(...)))[-1L]

  if (length(exprs) == 0L)
    rlang::abort("`chk_any()` needs at least one assertion.",
                 call = rlang::caller_env())
  nms <- names(exprs)
  if (!is.null(nms) && any(nzchar(nms)))
    rlang::abort("Arguments to `chk_any()` are assertions, and must not be named.",
                 call = rlang::caller_env())

  # checked before anything is evaluated, so that an argument that cannot assert
  # is an error whether or not an earlier branch would have passed first
  for (i in seq_along(exprs))
    if (!is.call(exprs[[i]])) chk_fail_not_a_call(exprs[[i]], i, length(exprs))

  # substitute() reaches through a forwarded `...` and hands back the original
  # expressions, but they were written a frame further up than the one they
  # would be evaluated in here, so the objects they name are the wrong ones or
  # no ones. The call as written still says `...` where substitute() does not.
  cl <- sys.call()
  for (a in as.list(cl)[-1L])
    if (identical(a, quote(...))) chk_fail_forwarded_dots()

  penv <- parent.frame()
  failures <- vector("list", length(exprs))
  for (i in seq_along(exprs)) {
    value <- eval(exprs[[i]], chk_suppress_data, penv)
    if (!is_chk_failure(value)) return(invisible(value))
    failures[[i]] <- value
  }
  chk_fail_any(failures, cl)
}

# The failure path for chk_any(), with the same shape as chk_fail(): report the
# collected bullets, unless this chk_any() is itself a branch of an enclosing
# one, in which case hand a failure object up instead. parent.frame(2) is the
# frame that called chk_any(), one above chk_any() itself, exactly as it is in
# chk_fail().
#
# The bullets carry no label of their own when every branch asserted on the
# same expression, which is the ordinary case; when the branches disagree, each
# bullet says which expression it belongs to, and the whole chk_any() call
# becomes the label if the failure travels further up.
chk_fail_any <- function(failures, cl, call = rlang::caller_env(2)) {
  labels <- vapply(failures, function(f) chk_arg_label(f$arg), character(1L))
  common <- length(unique(labels)) == 1L

  if (common) {
    bullets <- unlist(lapply(failures, function(f) f$bullets), use.names = FALSE)
    header  <- paste0("Assertion on `", labels[[1L]],
                      "` failed, none of the alternatives passed:")
    arg     <- labels[[1L]]
  } else {
    bullets <- unlist(Map(function(f, lab) paste0("`", lab, "`: ", f$bullets),
                          failures, labels), use.names = FALSE)
    header  <- "Assertion failed, none of the alternatives passed:"
    arg     <- deparse1(cl)
  }

  if (chk_suppressed(parent.frame(2)))
    return(chk_failure(bullets, arg))
  rlang::abort(c(header, rlang::set_names(bullets, "*")), call = call)
}

# Forwarding is refused rather than supported: recovering the environment each
# expression belongs to means rlang::enquos(), which costs an order of
# magnitude more than substitute() on the path that runs when an assertion
# passes. Silently evaluating in the wrong scope is the outcome being ruled out
# here -- a local `x` in the forwarding function's caller is invisible, and a
# global one of the same name is not, so the assertion can pass on an object
# the caller never meant.
chk_fail_forwarded_dots <- function() {
  rlang::abort(
    c("`chk_any()` cannot take a forwarded `...`.",
      i = paste0("Its arguments are evaluated in the calling environment, and ",
                 "a forwarded expression belongs to a frame above it."),
      i = "Write the assertions at the call site."),
    call = rlang::caller_env())
}

# An argument that is not a call cannot assert anything, and would otherwise be
# taken for a branch that passed. The reason it is usually there is a pipe: the
# native pipe inserts into the first argument, so `x |> chk_any(chk_string())`
# hands chk_any() the object where it expected an assertion, and would return it
# unchecked.
chk_fail_not_a_call <- function(expr, i, n) {
  msg <- c(paste0("`chk_any()` takes assertion calls, and `", deparse1(expr),
                  "` is not one."))
  if (i == 1L && n > 1L)
    msg <- c(msg, i = paste0("An object cannot be piped into `chk_any()`. ",
                             "Name it inside each assertion instead."))
  rlang::abort(msg, call = rlang::caller_env())
}

# A failure object carries the expression it was raised on, unless a caller of
# chk_fail() passed a label of its own.
chk_arg_label <- function(arg) {
  if (is.character(arg)) arg else deparse1(arg)
}
