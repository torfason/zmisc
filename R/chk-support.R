# R/chk-support.R -------------------------------------------------------------
#
# Hand-written companions to the generated chk_*() functions.
#
# 1. Parameter translation and the attribute policy.
# 2. The failure path.
# 3. The check_*() functions that checkmate does not provide. These use
#    checkmate's own argument names, so the generator reads them exactly as it
#    reads checkmate's, and they rewrite checkmate's messages where needed so
#    that a scalar reports failure the same way its family does.

# ---- Parameter translation ---------------------------------------------------

# First and last element of `v`, which is how both `length` and `range` are
# spelled out into a checkmate pair. A scalar therefore pins both ends. The
# argument is named `v` so that nothing shadows base::length() at the call
# site. Only ever reached on the parameterised path.
#
# A pair that nothing can satisfy is a mistake in the call rather than a
# property of `x`, so it is raised here instead of being routed through
# chk_fail(). That also keeps it an error inside chk_any(), where a failed
# assertion is only a branch that did not pass.
# A count has to be a number, since a non-number would otherwise be read as an
# absent bound and dropped, and it cannot be negative, since there is nothing
# below zero for it to admit. That takes -Inf with it: unbounded below is 0 for
# a count, so writing -Inf is a mistake in the same way -1 is, and checkmate
# rejects the one already. Value bounds are left alone on both counts. They are
# not always numeric -- a date range is a pair of Dates -- and a negative lower
# bound is ordinary there.
#
# NA at an end is no bound there, so the rules only apply to an end that is
# one. Both at once is not a pair of bounds at all, and it is the shape a range
# computed over missing data collapses to, so it is refused: NULL already says
# no bounds, and says it on purpose.
ends <- function(v, count, arg, call) {
  n  <- length(v)
  lo <- if (n > 0L) v[[1L]]
  hi <- if (n > 0L) v[[n]]
  set_lo <- n > 0L && !is.na(lo)
  set_hi <- n > 0L && !is.na(hi)
  bad <-
    if (n == 0L)                 "It must not be empty."
    else if (!set_lo && !set_hi) "Both ends are missing. `NULL` says that already."
    else if (count && ((set_lo && !is.numeric(lo)) || (set_hi && !is.numeric(hi))))
                                 "Both ends must be numeric."
    else if (count && set_lo && lo == Inf)
                                 "A lower bound of Inf can never be met."
    else if (count && ((set_lo && lo < 0) || (set_hi && hi < 0)))
                                 "Neither end may be negative."
    else if (set_lo && set_hi && lo > hi)
      paste0("The upper end (", hi, ") is below the lower end (", lo, ").")
  if (!is.null(bad))
    rlang::abort(c(paste0("`", arg, "` is not a usable pair of bounds."), x = bad),
                 call = call)
  list(lo, hi)
}

# An end that states no bound. NA says it whatever the type, and survives being
# written next to a Date; Inf says it too, wherever the type keeps its meaning
# that far.
unbounded <- function(e) is.na(e) || is.infinite(e)

# Bounds on values, for `lower` and `upper`. An unbounded end falls back to
# whatever this type would have used had none been given: Inf for the numeric
# types, whose checkmate defaults are already infinite, and NULL for the date
# and time types, where checkmate spells an absent bound that way and refuses
# an infinite date outright. `default` is that pair, and is required, so that a
# count cannot arrive here by mistake.
#
# The two ends are kept apart rather than combined, since combining them is
# what loses attributes: c() on a zoned POSIXct and anything that is not one
# drops `tzone`, and checkmate then reports that the bound and `x` disagree
# about it. Nothing here can undo that -- it happens in the call, before the
# pair arrives -- but nothing here adds to it either.
lo_hi <- function(v, default, arg = deparse1(substitute(v)),
                  call = rlang::caller_env()) {
  if (is.null(v)) return(default)
  e <- ends(v, FALSE, arg, call)
  list(if (unbounded(e[[1L]])) default[[1L]] else e[[1L]],
       if (unbounded(e[[2L]])) default[[2L]] else e[[2L]])
}

# Bounds on counts, for `len`/`min.len`/`max.len` and their `n.chars` twins.
# checkmate spells an absent count NULL, hence a list, where a vector could not
# have held one. Nothing at all comes back as NULL rather than as a list of
# them, since `$` reaches through NULL and gives the same answer for less. That
# is the common case, because the slow path reads the pair whether or not one
# was given.
#
# `exact` is the value both ends collapse to, or NULL if they differ. checkmate
# reports a min/max failure as ">= n" or "<= n", which reads wrong when a
# scalar pinned both ends, so the collapsed value goes to checkmate's
# exact-value argument as well as to the pair. NULL there is a no-op, so the
# pair alone still decides whenever the two ends differ.
lo_hi_count <- function(v, arg = deparse1(substitute(v)),
                        call = rlang::caller_env()) {
  if (is.null(v)) return(NULL)
  e  <- ends(v, TRUE, arg, call)
  lo <- e[[1L]]
  hi <- e[[2L]]
  list(exact = if (!unbounded(lo) && !unbounded(hi) && lo == hi) lo else NULL,
       min   = if (unbounded(lo)) NULL else lo,
       max   = if (unbounded(hi)) NULL else hi)
}

# ---- Attribute policy --------------------------------------------------------
#
# attr.ok is an allow-list of attribute names, or FALSE for none at all, or
# TRUE for any. `structural` names the attributes that are intrinsic to the
# type being checked and are therefore not the caller's to permit: "class" and
# "levels" for a factor, and so on. They are removed before the allow-list is
# applied, so attr.ok keeps the same meaning and the same "names" default for
# every type, and still means "no extras at all" when FALSE. For the bare
# types `structural` is empty and the default "names" is exactly is.vector(),
# which is what the generated fast path inlines. NULL carries no attributes,
# so the contract is vacuous for it and null.ok alone decides.

attrs_ok <- function(x, attr.ok, structural = character()) {
  if (isTRUE(attr.ok)) return(TRUE)
  nms <- setdiff(names(attributes(x)), structural)
  if (length(nms) == 0L) return(TRUE)
  if (isFALSE(attr.ok)) return(FALSE)
  !anyNA(match(nms, attr.ok))
}

bad_attrs <- function(x, attr.ok, structural = character()) {
  if (isTRUE(attr.ok)) return(character())
  nms <- setdiff(names(attributes(x)), structural)
  if (length(nms) == 0L) return(character())
  if (isFALSE(attr.ok)) return(nms)
  setdiff(nms, attr.ok)
}

# ---- Failure path ------------------------------------------------------------

# arg  : caller_arg(x) resolves the promise for `x` one frame up, in the
#        chk_*() function, and so yields the expression the user wrote.
# call : caller_env(2) is the frame that called chk_*(), so the error is
#        attributed to the user's function rather than to chk_*() itself.
# `dim` and `class` keep their own wording, since those are the two rejections
# that carry meaning for a reader; anything else is reported by name.
chk_fail <- function(x, res, attr.ok = TRUE, structural = character(),
                     arg = deparse1(substitute(x, parent.frame())),
                     call = rlang::caller_env(2)) {
  bullets <- character()
  if (!isTRUE(res))
    bullets <- c(bullets, res)
  bad <- bad_attrs(x, attr.ok, structural)
  if ("dim" %in% bad)
    bullets <- c(bullets, paste0("Must not have a dim attribute, but has dim ",
                                 deparse1(attr(x, "dim", exact = TRUE))))
  if ("class" %in% bad)
    bullets <- c(bullets, paste0("Must not have a class attribute, but has class ",
                                 deparse1(oldClass(x))))
  rest <- setdiff(bad, c("dim", "class"))
  if (length(rest) > 0L)
    bullets <- c(bullets, paste0("Must not have attributes: ", toString(rest)))
  if (length(bullets) == 0L)
    rlang::abort("chk_fail() reached with nothing to report.", .internal = TRUE)
  rlang::abort(
    c(paste0("Assertion on `", arg, "` failed:"),
      rlang::set_names(bullets, "*")),
    call = call
  )
}

# ---- check_*() functions not provided by checkmate ---------------------------
#
# Argument names and defaults here become the exposed API of the corresponding
# chk_*(), since the generator reads them off these signatures.
#
# Each rewrites checkmate's message so that the scalar reports a failure the
# same way checkmate's own scalar checks do: check_int() says "May not be NA"
# where check_integer() says "Contains missing values (element 1)".

check_inumber <- function(x, na.ok = FALSE, lower = -Inf, upper = Inf,
                          null.ok = FALSE) {
  result <- checkmate::check_integer(x, lower = lower, upper = upper, len = 1L,
                                     any.missing = na.ok, null.ok = null.ok)
  if (identical(result, "Contains missing values (element 1)")) {
    "May not be NA"
  } else {
    result
  }
}

check_dnumber <- function(x, na.ok = FALSE, lower = -Inf, upper = Inf,
                          null.ok = FALSE) {
  result <- checkmate::check_double(x, lower = lower, upper = upper, len = 1L,
                                    any.missing = na.ok, null.ok = null.ok)
  if (identical(result, "Contains missing values (element 1)")) {
    "May not be NA"
  } else {
    result
  }
}

# `lower` is exposed so that `range` maps here as it does elsewhere, but it is
# clamped at zero: naturalish is a type assertion, and `range` must not be able
# to widen it back into plain integerish. `positive` mirrors the argument of
# checkmate::check_count(), and is what the generator reads to give this type a
# zero.ok parameter. It raises the floor rather than replacing it, so that an
# explicit `range` and zero.ok = FALSE compose instead of one silently winning.
check_naturalish <- function(x, lower = 0, upper = Inf, positive = FALSE,
                             tol = sqrt(.Machine$double.eps),
                             any.missing = TRUE, all.missing = TRUE,
                             len = NULL, min.len = NULL, max.len = NULL,
                             unique = FALSE, sorted = FALSE, names = NULL,
                             null.ok = FALSE) {
  result <- checkmate::check_integerish(x, lower = max(lower, if (positive) 1 else 0),
                                        upper = upper,
                                        tol = tol, any.missing = any.missing,
                                        all.missing = all.missing, len = len,
                                        min.len = min.len, max.len = max.len,
                                        unique = unique, sorted = sorted,
                                        names = names, null.ok = null.ok)
  if (isTRUE(result)) result else sub("integerish", "naturalish", result)
}
