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
lo_hi <- function(v, default = NULL) {
  if (is.null(v)) return(default)
  c(v[[1L]], v[[length(v)]])
}

# The value both ends of `v` collapse to, or NULL if they differ. checkmate
# reports a min/max failure as ">= n" or "<= n", which reads wrong when a
# scalar pinned both ends, so the collapsed value is passed to checkmate's
# exact-value argument (`len`, `n.chars`) as well as to the pair. NULL there is
# a no-op, so the pair alone still decides whenever the two ends differ.
exact <- function(v) {
  if (is.null(v) || v[[1L]] != v[[length(v)]]) return(NULL)
  v[[1L]]
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
# to widen it back into plain integerish.
check_naturalish <- function(x, lower = 0, upper = Inf,
                             tol = sqrt(.Machine$double.eps),
                             any.missing = TRUE, all.missing = TRUE,
                             len = NULL, min.len = NULL, max.len = NULL,
                             unique = FALSE, sorted = FALSE, names = NULL,
                             null.ok = FALSE) {
  result <- checkmate::check_integerish(x, lower = max(lower, 0), upper = upper,
                                        tol = tol, any.missing = any.missing,
                                        all.missing = all.missing, len = len,
                                        min.len = min.len, max.len = max.len,
                                        unique = unique, sorted = sorted,
                                        names = names, null.ok = null.ok)
  if (isTRUE(result)) result else sub("integerish", "naturalish", result)
}
