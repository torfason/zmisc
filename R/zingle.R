#' Return the single (unique) value found in a vector
#'
#' @description
#' [zingle()] returns the only value present in a vector. If the vector contains
#' more than one distinct value, it throws an error. This is a guard for
#' aggregations where all values within a group should be identical, but where
#' you want that assumption checked rather than assumed. Only values are
#' compared. Names are ignored and the result is unnamed, in line with other
#' aggregation functions.
#'
#'
#' @param x Vector of elements that should all be identical
#' @param ... Unused, reserved to force later arguments to be named
#' @param empty.ok Is an empty vector ok?
#' @param na.ok.partial Is a mix of `NA` and one distinct non-missing value ok?
#' @param na.ok.all Is a vector of only `NA` values ok?
#' @param nan.ok.all Is `NaN` ok as the returned value?
#' @return The single element in the vector, unnamed. For `na.ok.partial` the
#'   non-missing value is returned.
#'
#' @examples
#' zingle(c("Alpha", "Alpha", "Alpha"))
#' zingle(c("Alpha", NA, "Alpha"), na.ok.partial = TRUE)
#' zingle(c(NA, NA), na.ok.all = TRUE)
#' zingle(c(NaN, NaN), nan.ok.all = TRUE)
#'
#' @export
zingle <- function(x, ...,
                   empty.ok      = FALSE,
                   na.ok.partial = FALSE,
                   na.ok.all     = FALSE,
                   nan.ok.all    = FALSE) {

  rlang::check_dots_empty()

  if (!is.atomic(x) || is.null(x) || !is.null(dim(x))) {
    hint <- if (inherits(x, "POSIXlt")) {
      "Use `as.POSIXct(x)` to convert it first."
    } else if (is.data.frame(x)) {
      "Did you mean to pass a single column?"
    } else {
      NULL
    }
    rlang::abort(c(
      paste0("`x` must be an atomic vector, not ", class(x)[1], "."),
      i = hint
    ))
  }

  # unique() does the equality work, and keeps NA and NaN as distinct elements
  # in their own right, so there is at most one of each. Names are dropped
  # once, here, so every return below is unnamed.
  u   <- unname(unique(x))
  na  <- is.na(u) & !is.nan(u)
  val <- u[!na]

  if (length(u) == 0L) {
    if (!empty.ok)
      rlang::abort(c("`x` must not be empty.",
                     i = "Set `empty.ok = TRUE` to allow this."))
    return(u[NA_integer_])
  }

  if (length(val) > 1L)
    rlang::abort(paste0(
      "`x` must contain a single unique value, but found ",
      length(u), " distinct values."
    ))

  if (length(val) == 0L) {
    if (!na.ok.all)
      rlang::abort(c("`x` must not consist entirely of missing values.",
                     i = "Set `na.ok.all = TRUE` to allow this."))
    return(u[NA_integer_])
  }

  if (any(na) && !na.ok.partial)
    rlang::abort(c(
      paste0("`x` must not contain missing values, but ",
             sum(is.na(x) & !is.nan(x)), " of ", length(x), " are missing."),
      i = "Set `na.ok.partial = TRUE` to allow this."
    ))

  if (is.nan(val) && !nan.ok.all)
    rlang::abort(c("The only value in `x` is `NaN`.",
                   i = "Set `nan.ok.all = TRUE` to allow this."))

  val
}
