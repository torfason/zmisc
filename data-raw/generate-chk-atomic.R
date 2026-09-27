## data-raw/generate-chk-atomic.R ---------------------------------------------------
##
## Generates R/chk-1-atomic.R, covering the unclassed (bare) types only.
##
## Usage:
##   pkgload::load_all()
##   source("data-raw/generate-chk-atomic.R")
##   write_chk()
##
## render_chk() returns the file contents as a character vector; write_chk()
## writes them to disk. Nothing checks that the file on disk is current, so
## write_chk() belongs at the head of the build, as it is in
## build/build_and_release_process.R.
##
## Each generated function exposes one cohesive parameter set:
##
##   na.ok     missing values permitted; mapped to any.missing for the vector
##             checks and to na.ok for the scalar ones, taking checkmate's
##             default in each case (TRUE and FALSE respectively)
##   zero.ok   zero permitted; the negation of checkmate's `positive`, which
##             only check_count() and check_naturalish() have, so only the
##             count and naturalish types get it
##   null.ok   passed straight through
##   attr.ok   which attributes x may carry beyond those intrinsic to the type,
##             enforced here rather than by checkmate: a character vector of
##             permitted names, defaulting to "names"; FALSE for none; TRUE for
##             any. The intrinsic ones come from the `attrs` spec column and
##             are subtracted before the allow-list applies, so the default is
##             "names" for a classed type as much as for a bare one
##   length    NULL, or a vector: first element to min.len, last to max.len,
##             so a scalar means one permitted length, and is passed to `len`
##             as well so that checkmate reports it as an exact length. Counts
##             go through lo_hi_count(), which turns an unbounded end into the
##             NULL checkmate expects and refuses a pair nothing can satisfy
##   range     same first/last rule, mapped to lower/upper, or to
##             min.chars/max.chars (and `n.chars`) for the character types.
##             The character ends are counts and take lo_hi_count() too; the
##             bounds are doubles and keep their Inf
##
## Which of these a given chk_*() gets is derived from the formals of its
## backing check_*(), not from a spec column. Every remaining checkmate
## argument is pinned at its checkmate default and listed in a comment above
## the function it belongs to.
##
## Two paths through each generated body:
##
##   nargs() == 1  a cheap sufficient predicate, usually is.vector(x, <mode>),
##                 standing in for the backing check at its own defaults and
##                 for attr.ok = "names", with no argument construction at
##                 all; see render_fast()
##   nargs()  > 1  parameters translated into checkmate's vocabulary, and
##                 attrs_ok() to honour whatever attr.ok was given

library(tibble)
library(purrr)
library(glue)

# glue with {{ }} delimiters throughout, so the braces of the generated R code
# pass through untouched. .envir must be forwarded or templates would be
# evaluated in this frame.
glu <- function(..., .envir = parent.frame()) {
  glue(..., .open = "{{", .close = "}}", .envir = .envir)
}

# ---- Specification ---------------------------------------------------------

# name  : the chk_<name>() to generate
# kind  : "scalar" or "vector"; carried into the generated file as a comment
# check : the check_*() function that does the work
# attrs : attributes intrinsic to the type, exempt from the attr.ok contract
# mode  : the is.vector() mode that gives the fast path a cheap sufficient
#         predicate, or NA to keep the backing check there; see render_fast()
# extra : one further fast-path clause the type needs, or NA

chk_spec <- tribble(
  ~name,        ~kind,    ~check,             ~attrs,               ~mode,       ~extra,
  "flag",       "scalar", "check_flag",       character(),          "logical",   NA,
  "logical",    "vector", "check_logical",    character(),          "logical",   NA,
  "string",     "scalar", "check_string",     character(),          "character", NA,
  "character",  "vector", "check_character",  character(),          "character", NA,
  "number",     "scalar", "check_number",     character(),          "numeric",   NA,
  "numeric",    "vector", "check_numeric",    character(),          "numeric",   NA,
  "inumber",    "scalar", "check_inumber",    character(),          "integer",   NA,
  "integer",    "vector", "check_integer",    character(),          "integer",   NA,
  "dnumber",    "scalar", "check_dnumber",    character(),          "double",    NA,
  "double",     "vector", "check_double",     character(),          "double",    NA,
  "znumber",    "scalar", "check_int",        character(),          "integer",   NA,
  "integerish", "vector", "check_integerish", character(),          "integer",   NA,
  "count",      "scalar", "check_count",      character(),          "integer",   "x >= 0L",
  "naturalish", "vector", "check_naturalish", character(),          NA,          NA,
  "factor",     "vector", "check_factor",     c("class", "levels"), NA,          NA,
  "complex",    "vector", "check_complex",    character(),          "complex",   NA,
  "raw",        "vector", "check_raw",        character(),          "raw",       NA,
  "day",        "scalar", "check_day",        "class",              NA,          NA,
  "date",       "vector", "check_date",       "class",              NA,          NA,
  "instant",    "scalar", "check_instant",    c("class", "tzone"),  NA,          NA,
  "posixct",    "vector", "check_posixct",    c("class", "tzone"),  NA,          NA,
  "scalar",     "scalar", "check_scalar",     character(),          NA,          NA,
  "atomic",     "vector", "check_atomic",     character(),          NA,          NA
)

param_desc <- c(
  x        = "Object to check.",
  ...      = "These dots are for future extensions and must be empty.",
  na.ok    = "Are missing values permitted?",
  zero.ok  = "Is zero permitted?",
  null.ok  = "Is `NULL` permitted?",
  attr.ok  = paste("Which attributes `x` may carry beyond those intrinsic to",
                   "its type: a character vector of permitted attribute names,",
                   "`FALSE` for none at all, or `TRUE` for any."),
  length   = paste("Permitted length. `NULL` for any length, a scalar for one",
                   "exact length, or a vector whose first and last elements",
                   "give the minimum and the maximum. Neither may be",
                   "negative, and `NA` at an end, or `Inf` as the maximum,",
                   "means no bound there."),
  range    = paste("Permitted range of values, under the same first/last rule",
                   "as `length`. For the character types it constrains",
                   "`nchar()` of the elements instead, and for the date and",
                   "time types the bounds are themselves `Date` or `POSIXct`.",
                   "`NA` at an end means no bound there, and so does an",
                   "infinite end wherever the type keeps that meaning.")
)

# ---- Reading the backing signatures ----------------------------------------

fold <- function(x) {
  if (length(x))
    glue_collapse(x, "\n")
  else
    ""
}

resolve_check <- function(check) {
  ns <- asNamespace("zmisc")
  if (!exists(check, envir = ns, inherits = TRUE))
    stop(glu("No backing function found for `{{check}}`"), call. = FALSE)
  get(check, envir = ns, inherits = TRUE)
}

arg_table <- function(check) {
  fmls <- as.list(formals(resolve_check(check)))
  fmls <- fmls[setdiff(names(fmls), "x")]
  tibble(
    cm   = names(fmls),
    dflt = map_chr(fmls, \(d) if (rlang::is_missing(d)) "" else deparse1(d))
  )
}

# Decide, from the backing signature alone, which parameters this chk_*() gets
# and which checkmate arguments are left pinned at their defaults.
plan_args <- function(check) {
  a    <- arg_table(check)
  has  <- \(nm) nm %in% a$cm
  dflt <- \(nm) a$dflt[[match(nm, a$cm)]]

  na_cm <- if (has("any.missing")) "any.missing" else if (has("na.ok")) "na.ok" else NA_character_
  range <- if (has("lower") && has("upper")) "bounds"
  else if (has("min.chars") && has("max.chars")) "chars"
  else NA_character_
  len   <- has("min.len") && has("max.len")
  zero  <- has("positive")

  bounds <- if (identical(range, "bounds")) {
    lo <- dflt("lower")
    hi <- dflt("upper")
    if (lo == "NULL" && hi == "NULL") "NULL" else glu("c({{lo}}, {{hi}})")
  } else NA_character_

  used <- c(
    na_cm,
    if (zero) "positive",
    if (has("null.ok")) "null.ok",
    if (len) c("len", "min.len", "max.len"),
    if (identical(range, "bounds")) c("lower", "upper"),
    if (identical(range, "chars")) c("n.chars", "min.chars", "max.chars")
  )
  used <- used[!is.na(used)]

  list(
    na_cm     = na_cm,
    na_dflt   = if (is.na(na_cm)) NA_character_ else dflt(na_cm),
    zero      = zero,
    # zero.ok is the negation of checkmate's `positive`, default included
    zero_dflt = if (zero) deparse1(!as.logical(dflt("positive"))) else NA_character_,
    null_ok   = has("null.ok"),
    null_dflt = if (has("null.ok")) dflt("null.ok") else NA_character_,
    len       = len,
    range     = range,
    bounds    = bounds,
    pinned    = set_names(a$dflt[!a$cm %in% used], a$cm[!a$cm %in% used])
  )
}

# ---- Rendering -------------------------------------------------------------

render_signature <- function(name, p) {
  args <- c(
    "x",
    "...",
    if (!is.na(p$na_cm)) glu("na.ok = {{p$na_dflt}}"),
    if (p$zero) glu("zero.ok = {{p$zero_dflt}}"),
    if (p$null_ok) glu("null.ok = {{p$null_dflt}}"),
    r"---(attr.ok = "names")---",
    if (p$len) "length = NULL",
    if (!is.na(p$range)) "range = NULL"
  )
  sig <- glue_collapse(args, ", ")
  glu(r"---(chk_{{name}} <- function({{sig}}) {)---")
}

render_locals <- function(p) {
  c(
    if (p$len) "  len <- lo_hi_count(length)",
    if (identical(p$range, "bounds")) glu("  rng <- lo_hi(range, {{p$bounds}})"),
    if (identical(p$range, "chars"))  "  rng <- lo_hi_count(range)"
  )
}

render_slow_call <- function(check, p) {
  pairs <- c(
    if (!is.na(p$na_cm)) glu("{{p$na_cm}} = na.ok"),
    if (p$zero) "positive = !zero.ok",
    if (p$null_ok) "null.ok = null.ok",
    if (p$len) c("len = len$exact", "min.len = len$min", "max.len = len$max"),
    if (identical(p$range, "bounds")) c("lower = rng[[1L]]", "upper = rng[[2L]]"),
    if (identical(p$range, "chars")) c("n.chars = rng$exact", "min.chars = rng$min",
                                       "max.chars = rng$max")
  )
  head <- glu("  res <- {{check}}(x")
  if (length(pairs) == 0L) return(glu("{{head}})"))
  indent <- strrep(" ", nchar(head) - 1L)
  ends <- c(rep(",", length(pairs) - 1L), ")")
  c(glu("{{head}},"),
    glu("{{indent}}{{pairs}}{{ends}}"))
}

render_pinned <- function(p) {
  if (length(p$pinned) == 0L) return(character())
  txt  <- glue_collapse(glu("{{names(p$pinned)}} = {{p$pinned}}"), ", ")
  wrap <- strwrap(glu("pinned: {{txt}}"), width = 76, exdent = 2)
  glu("# {{wrap}}")
}



# A fast path only has to be sound, never complete. Whatever it rejects falls
# through to the full check_*() call and gets the same answer, one order of
# magnitude later, which is the licence for putting a cheap sufficient
# condition where the semantic check used to be.
#
# is.vector(x, "<mode>") is TRUE only for a vector of that mode carrying no
# attributes beyond names, so it implies both check_*(x) at its own defaults
# and attrs_ok(x, "names"), at about a sixth of the cost of the two calls it
# replaces. The scalar types add length(x) == 1L && !is.na(x), sound because
# the fast path only fires at nargs() == 1L, where na.ok is still at its
# default FALSE; is.na() covers NaN, as check_number() does.
#
# Incompleteness costs a fast path and nothing else. is.vector(3, "integer")
# is FALSE, so chk_znumber(3) passes by the slow path, and so does
# chk_character(NA), which check_character() accepts on typed.missing = FALSE.
#
# mode = NA keeps the semantic call, for the types with no sufficient
# condition of this shape: naturalish, which needs a value scan that is
# plausibly dearer than checkmate's C loop; scalar and atomic, whose shape is
# not a mode at all, is.vector(x, "any") being TRUE for lists too; and the
# classed types, which want is.factor() or inherits() and so a class column
# rather than a mode.
render_fast <- function(kind, check, attrs, mode, extra) {
  if (length(attrs) > 0L)
    glu(r"---(isTRUE({{check}}(x)) && attrs_ok(x, "names", {{deparse1(attrs)}}))---")
  else if (is.na(mode))
    glu(r"---(isTRUE({{check}}(x)) && is.vector(x, "any"))---")
  else
    glue_collapse(c(
      glu(r"---(is.vector(x, "{{mode}}"))---"),
      if (kind == "scalar") c("length(x) == 1L", "!is.na(x)"),
      if (!is.na(extra)) extra
    ), " && ")
}

render_fun <- function(name, kind, check, attrs, mode, extra) {
  p <- plan_args(check)

  # The scalar clauses in render_fast() are sound only for as long as the
  # backing check leaves na.ok FALSE at its default.
  if (kind == "scalar" && !is.na(mode) && !identical(p$na_dflt, "FALSE"))
    stop(glu("Fast path for `{{check}}()` assumes na.ok = FALSE, not {{p$na_dflt}}"),
         call. = FALSE)

  fast <- render_fast(kind, check, attrs, mode, extra)
  # The slow path names the intrinsic attributes so that attrs_ok() and
  # chk_fail() can subtract them before the attr.ok allow-list applies.
  strc <- if (length(attrs) == 0L) "" else glu(", {{deparse1(attrs)}}")

  glu(r"---(
    # chk_{{name}}(): {{kind}}, backed by {{check}}()
    {{fold(render_pinned(p))}}
    #' @rdname chk_atomic
    #' @export
    {{fold(render_signature(name, p))}}

      # No arguments, return on fastest path
      if (nargs() == 1L && {{fast}})
          return(invisible(x))

      # Anything in the dots is a typo, not an extension
      chk_dots_empty()

      # More detailed translation of arguments to check_*() equivalents
    {{fold(render_locals(p))}}
    {{fold(render_slow_call(check, p))}}
      if (isTRUE(res) && attrs_ok(x, attr.ok{{strc}})) return(invisible(x))
      chk_fail(x, res, attr.ok{{strc}})
    }
    )---")
}

# All functions share one help topic, so the union of argument names has to be
# documented once or R CMD check reports undocumented arguments.
render_params <- function(spec) {
  used <- spec$check |> map(plan_args)
  nms <- c(
    "x",
    "...",
    if (any(map_lgl(used, \(p) !is.na(p$na_cm)))) "na.ok",
    if (any(map_lgl(used, "zero"))) "zero.ok",
    if (any(map_lgl(used, "null_ok"))) "null.ok",
    "attr.ok",
    if (any(map_lgl(used, "len"))) "length",
    if (any(map_lgl(used, \(p) !is.na(p$range)))) "range"
  )
  c(glu("#' @param {{nms}} {{param_desc[nms]}}"),
    "#' @rdname chk_atomic",
    "#' @name chk_atomic",
    "NULL", "")
}

render_chk <- function(spec = chk_spec) {
  c(
    "##",
    "## GENERATED BY data-raw/generate-chk-atomic.R",
    "##",
    "## DO NOT EDIT BY HAND",
    "##",
    "",
    render_params(spec),
    pmap(spec, render_fun) |> unlist()
  )
}

write_chk <- function(path = "R/chk-1-atomic.R") {
  writeLines(render_chk(), path)
  invisible(path)
}
