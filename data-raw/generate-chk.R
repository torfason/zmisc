## data-raw/generate-chk.R ---------------------------------------------------
##
## Generates R/chk-generated.R, covering the unclassed (bare) types only.
##
## Usage:
##   pkgload::load_all()
##   source("data-raw/generate-chk.R")
##   write_chk()
##
## render_chk() returns the file contents as a character vector; write_chk()
## writes them to disk. tests/testthat/test-chk-generated.R compares the
## checked-in file against a fresh render_chk(), so stale generation fails the
## test suite rather than shipping.
##
## Each generated function exposes one cohesive parameter set:
##
##   na.ok     missing values permitted; mapped to any.missing for the vector
##             checks and to na.ok for the scalar ones, taking checkmate's
##             default in each case (TRUE and FALSE respectively)
##   null.ok   passed straight through
##   attr.ok   which attributes x may carry, enforced here rather than by
##             checkmate: a character vector of permitted names, defaulting to
##             "names" and so to is.vector(); FALSE for none; TRUE for any
##   length    NULL, or a vector: first element to min.len, last to max.len,
##             so a scalar means one permitted length
##   range     same first/last rule, mapped to lower/upper, or to
##             min.chars/max.chars for the character types
##
## Which of these a given chk_*() gets is derived from the formals of its
## backing check_*(), not from a spec column. Every remaining checkmate
## argument is pinned at its checkmate default and listed in a comment above
## the function it belongs to.
##
## Two paths through each generated body:
##
##   nargs() == 1  the backing check with its own defaults, plus is.vector(),
##                 which is exactly attr.ok = "names", and no argument
##                 construction at all
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

chk_spec <- tribble(
  ~name,        ~kind,    ~check,
  "flag",       "scalar", "check_flag",
  "logical",    "vector", "check_logical",
  "string",     "scalar", "check_string",
  "character",  "vector", "check_character",
  "number",     "scalar", "check_number",
  "numeric",    "vector", "check_numeric",
  "inumber",    "scalar", "check_inumber",
  "integer",    "vector", "check_integer",
  "dnumber",    "scalar", "check_dnumber",
  "double",     "vector", "check_double",
  "znumber",    "scalar", "check_int",
  "integerish", "vector", "check_integerish",
  "count",      "scalar", "check_count",
  "naturalish", "vector", "check_naturalish",
  "complex",    "vector", "check_complex",
  "raw",        "vector", "check_raw",
  "scalar",     "scalar", "check_scalar",
  "atomic",     "vector", "check_atomic"
)

param_desc <- c(
  x        = "Object to check.",
  ...      = "Reserved.",
  na.ok    = "Are missing values permitted?",
  null.ok  = "Is `NULL` permitted?",
  attr.ok  = paste("Which attributes `x` may carry: a character vector of",
                   "permitted attribute names, `FALSE` for none at all, or",
                   "`TRUE` for any."),
  length   = paste("Permitted length. `NULL` for any length, a scalar for one",
                   "exact length, or a vector whose first and last elements",
                   "give the minimum and the maximum."),
  range    = paste("Permitted range of values, under the same first/last rule",
                   "as `length`. For the character types it constrains",
                   "`nchar()` of the elements instead.")
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

  used <- c(
    na_cm,
    if (has("null.ok")) "null.ok",
    if (len) c("min.len", "max.len"),
    if (identical(range, "bounds")) c("lower", "upper"),
    if (identical(range, "chars")) c("min.chars", "max.chars")
  )
  used <- used[!is.na(used)]

  list(
    na_cm     = na_cm,
    na_dflt   = if (is.na(na_cm)) NA_character_ else dflt(na_cm),
    null_ok   = has("null.ok"),
    null_dflt = if (has("null.ok")) dflt("null.ok") else NA_character_,
    len       = len,
    range     = range,
    pinned    = set_names(a$dflt[!a$cm %in% used], a$cm[!a$cm %in% used])
  )
}

# ---- Rendering -------------------------------------------------------------

render_signature <- function(name, p) {
  args <- c(
    "x",
    "...",
    if (!is.na(p$na_cm)) glu("na.ok = {{p$na_dflt}}"),
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
    if (p$len) "  len <- lo_hi(length)",
    if (identical(p$range, "bounds")) "  rng <- lo_hi(range, c(-Inf, Inf))",
    if (identical(p$range, "chars"))  "  rng <- lo_hi(range)"
  )
}

render_slow_call <- function(check, p) {
  pairs <- c(
    if (!is.na(p$na_cm)) glu("{{p$na_cm}} = na.ok"),
    if (p$null_ok) "null.ok = null.ok",
    if (p$len) c("min.len = len[1L]", "max.len = len[2L]"),
    if (identical(p$range, "bounds")) c("lower = rng[1L]", "upper = rng[2L]"),
    if (identical(p$range, "chars")) c("min.chars = rng[1L]", "max.chars = rng[2L]")
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



render_fun <- function(name, kind, check) {
  p <- plan_args(check)
  glu(r"---(
    # chk_{{name}}(): {{kind}}, backed by {{check}}()
    {{fold(render_pinned(p))}}
    #' @rdname checkmate_rlang
    #' @export
    {{fold(render_signature(name, p))}}

      # No arguments, return on fastest path
      if (nargs() == 1L && isTRUE({{check}}(x)) && (is.vector(x, "any")) )
          return(invisible(x))

      # More detailed translation of arguments to check_*() equivalents
    {{fold(render_locals(p))}}
    {{fold(render_slow_call(check, p))}}
      if (isTRUE(res) && attrs_ok(x, attr.ok)) return(invisible(x))
      chk_fail(x, res, attr.ok)
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
    if (any(map_lgl(used, "null_ok"))) "null.ok",
    "attr.ok",
    if (any(map_lgl(used, "len"))) "length",
    if (any(map_lgl(used, \(p) !is.na(p$range)))) "range"
  )
  c(glu("#' @param {{nms}} {{param_desc[nms]}}"),
    "#' @name checkmate_rlang", "NULL", "")
}

render_chk <- function(spec = chk_spec) {
  c(
    "# Generated by data-raw/generate-chk.R -- do not edit by hand.",
    "",
    render_params(spec),
    pmap(spec, \(name, kind, check) render_fun(name, kind, check)) |> unlist()
  )
}

write_chk <- function(path = "R/chk-generated.R") {
  writeLines(render_chk(), path)
  invisible(path)
}
