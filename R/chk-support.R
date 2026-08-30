# R/chk-support.R -------------------------------------------------------------
#
# Hand-written companions to the generated chk_*() functions.
#
# 1. The failure path, shared by every generated function.
# 2. The three check_*() functions that checkmate does not provide. These use
#    checkmate's dotted argument convention, so the generator converts their
#    names to underscores exactly as it does for checkmate's own.

# ---- Failure path ------------------------------------------------------------

# arg  : caller_arg(x) resolves the promise for `x` one frame up, in the
#        chk_*() function, and so yields the expression the user wrote.
# call : caller_env(2) is the frame that called chk_*(), so the error is
#        attributed to the user's function rather than to chk_*() itself.
chk_fail <- function(x, res, dim_ok, class_ok,
                     arg = rlang::caller_arg(x),
                     call = rlang::caller_env(2)) {
  bullets <- character()
  if (!isTRUE(res))
    bullets <- c(bullets, res)
  if (!dim_ok && !is.null(attr(x, "dim", exact = TRUE)))
    bullets <- c(bullets, paste0("Must not have a dim attribute, but has dim ",
                                 deparse1(attr(x, "dim", exact = TRUE))))
  if (!class_ok && is.object(x))
    bullets <- c(bullets, paste0("Must not have a class attribute, but has class ",
                                 deparse1(oldClass(x))))
  rlang::abort(
    c(paste0("Assertion on `", arg, "` failed:"),
      rlang::set_names(bullets, "*")),
    call = call
  )
}

# ---- check_*() functions not provided by checkmate ---------------------------
#
# Signatures here define the exposed API of the corresponding chk_*(), so they
# are worth tuning deliberately. Dotted names on purpose.

check_inumber <- function(x, na.ok = FALSE, lower = -Inf, upper = Inf,
                          null.ok = FALSE) {
  result <- checkmate::check_integer(x, lower = lower, upper = upper, len = 1L,
                           any.missing = na.ok, null.ok = null.ok)
  if (result == "Contains missing values (element 1)") {
    return("May not be NA")
  }  else {
    return(result)
  }
}

check_dnumber <- function(x, na.ok = FALSE, lower = -Inf, upper = Inf,
                          null.ok = FALSE) {
  result <- checkmate::check_double(x, lower = lower, upper = upper, len = 1L,
                          any.missing = na.ok, null.ok = null.ok)
  if (result == "Contains missing values (element 1)") {
    return("May not be NA")
  }  else {
    return(result)
  }
}

check_naturalish <- function(x, tol = sqrt(.Machine$double.eps), upper = Inf,
                             any.missing = TRUE, all.missing = TRUE,
                             len = NULL, min.len = NULL, max.len = NULL,
                             unique = FALSE, sorted = FALSE, names = NULL,
                             null.ok = FALSE) {
  result <- checkmate::check_integerish(x, lower = 0, upper = upper, tol = tol,
                              any.missing = any.missing,
                              all.missing = all.missing, len = len,
                              min.len = min.len, max.len = max.len,
                              unique = unique, sorted = sorted, names = names,
                              null.ok = null.ok)
  ifelse(isTRUE(result), result, sub(x = result, "integerish", "naturalish"))
}
