## data-raw/generate-chk.R ---------------------------------------------------
##
## Generates R/chk-generated.R, covering the unclassed (bare) types only.
##
## Usage:
##   pkgload::load_all()
##   source("data-raw/generate-chk.R") OR
##   source("scratch/gen_assertions/generate-chk.R")
##   write_chk()
##
## render_chk() returns the file contents as a character vector; write_chk()
## writes them to disk. tests/testthat/test-chk-generated.R compares the
## checked-in file against a fresh render_chk(), so stale generation fails the
## test suite rather than shipping.
##
## Argument names and defaults are read off the backing check_*() function with
## formals(). The dotted checkmate names are therefore the single source of
## truth and the underscore names are derived from them. Adding an assertion is
## one row in chk_spec; if the backing function does not exist, generation
## fails immediately.
##
## The classed types -- factor, Date, POSIXct and their scalar forms -- are
## deliberately out of scope and stay hand-written. Their class attribute is
## their type assertion, so class.ok has no coherent meaning there.

library(tibble)
library(purrr)

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

# Checkmate arguments never exposed; checkmate's own default applies to these.
drop_args <- character()

# Hand-written @param text. Anything not listed falls back to a generic line.
param_desc <- c(
  x        = "Object to check.",
  dim.ok   = paste("If `FALSE` (the default), `x` must not carry a `dim`",
                   "attribute."),
  class.ok = paste("If `FALSE` (the default), `x` must not carry a `class`",
                   "attribute.")
)

# ---- Reading the backing signatures ----------------------------------------

resolve_check <- function(check) {
  ns <- asNamespace("zmisc")
  if (!exists(check, envir = ns, inherits = TRUE))
    stop("No backing function found for `", check, "`", call. = FALSE)
  get(check, envir = ns, inherits = TRUE)
}

arg_table <- function(check) {
  fmls <- as.list(formals(resolve_check(check)))
  fmls <- fmls[setdiff(names(fmls), c("x", drop_args))]
  tibble(
    cm   = names(fmls),

    # Swap these to change parameters from using dots to underscores
    #r    = gsub(".", "_", names(fmls), fixed = TRUE),
    r    = gsub(".", ".", names(fmls), fixed = TRUE),

    dflt = map_chr(fmls, \(d) if (rlang::is_missing(d)) "" else deparse1(d))
  )
}

# ---- Rendering -------------------------------------------------------------

render_signature <- function(args) {
  fmls <- ifelse(nzchar(args$dflt), paste0(args$r, " = ", args$dflt), args$r)
  paste(c("x", fmls, "dim.ok = FALSE", "class.ok = FALSE"), collapse = ", ")
}

render_call <- function(check, args) {
  if (nrow(args) == 0L) return(paste0(check, "(x)"))
  paste0(check, "(x, ", paste0(args$cm, " = ", args$r, collapse = ", "), ")")
}

render_fun <- function(name, kind, check) {
  args <- arg_table(check)
  c(
    paste0("# chk_", name, "(): ", kind, ", backed by ", check, "()"),
    "#' @rdname checkmate_rlang",
    "#' @export",
    paste0("chk_", name, " <- function(", render_signature(args), ") {"),
    paste0("  res <- ", render_call(check, args)),
    "  if (isTRUE(res) &&",
    "      (dim.ok || is.null(attr(x, \"dim\", exact = TRUE))) &&",
    "      (class.ok || !is.object(x)))",
    "    return(invisible(x))",
    "  chk_fail(x, res, dim.ok, class.ok)",
    "}",
    ""
  )
}

# All functions share one help topic, so the union of argument names has to be
# documented once or R CMD check reports undocumented arguments.
render_params <- function(spec) {
  nms <- spec$check |>
    map(arg_table) |>
    map("r") |>
    unlist() |>
    c("x", "dim.ok", "class.ok") |>
    unique() |>
    sort()
  desc <- unname(param_desc[nms])
  fallback <- paste0("Passed to the backing `check_*()` function as `",

                      # Swap these to change parameters from using dots to underscores
                     gsub("_", ".", nms, fixed = TRUE), "`.")
                     #gsub("_", ".", nms, fixed = TRUE), "`.")


  desc <- ifelse(is.na(desc), fallback, desc)
  c(paste0("#' @param ", nms, " ", desc), "#' @name checkmate_rlang",
    "NULL", "")
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
