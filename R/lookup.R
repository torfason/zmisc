
#' Lookup values from a lookup table
#'
#' @description
#' The [lookup()] function implements lookup of values (such as variable names)
#' from a lookup table which maps keys onto values (such as variable labels or
#' descriptions).
#'
#' The lookup table can be in the form of a two-column `data.frame`, in the form
#' of a named `vector`, or in the form of a `list`. If the table is in the form
#' of a `data.frame`, the key column should be named either `key` or `name`, and
#' the value column should be named `value` (for the value). If the lookup table
#' is in the form of a named `vector` or `list`, the names are used as the key,
#' and the returned value is taken from the values in the vector or list.
#'
#' The underlying lookup is done using `base::match()`, and all atomic data
#' types except `factor` are supported. Factors are omitted due to the ambiguity
#' in what should be looked up (the values or the levels). It is important that
#' `x`, `.default` and the columns of `lookup_table` are all of the same type
#' (specifically of the same `base::mode()`). If the lookup table is specified
#' as a `vector` or `list`, only the `character` variables are supported,
#' because `name(lookup_table)` is always of mode `character`.
#'
#' Original values are returned if they are not found in the lookup table.
#' Alternatively, a `.default` can be specified for values that are not found.
#' Note that it is possible to specify `NA` as one of the keys to look up
#' NA values (only when using a `data.frame` as lookup table).
#'
#' Any names or attributes of x are preserved.
#'
#' @param x A vector whose elements are to be looked up.
#'
#' @param lookup_table The lookup table to use.
#'
#' @param ... Reserved for future use.
#'
#' @param .default If a value is not found in the lookup table, the value will
#'   be taken from `.default`. This must be a vector of the same mode as x, and
#'   either of length 1 or the same length as x. Useful values include `x` (the
#'   default setting), `NA`, or `""` (an empty string). Specifying `.default =
#'   NULL` implies that `x` will be used for missing values.
#'
#' @return The [lookup()] function returns a vector based on `x`, with
#'   values replaced with the lookup values from `lookup_table`. Any values not
#'   found in the lookup table are taken from `.default`.
#'
#' @examples
#' fruit_lookup_vector <- c(a = "Apple", b = "Banana", c = "Cherry")
#' lookup(letters[1:5], fruit_lookup_vector)
#' lookup(letters[1:5], fruit_lookup_vector, .default = NA)
#'
#' mtcars_lookup_data_frame <- data.frame(
#'   name = c("mpg", "hp", "wt"),
#'   value = c("Miles/(US) gallon", "Gross horsepower", "Weight (1000 lbs)"))
#' lookup(names(mtcars), mtcars_lookup_data_frame)
#'
#' # A more complex example, with numeric and NA values
#' numeric_lookup_table <- data.frame(
#'   key = c(1:5, NA), value = c(sqrt(1:5), 99999))
#' lookup(c(0:6, NA), numeric_lookup_table)
#'
#' @export
lookup <- function(x, lookup_table, ..., .default = x) {

  # NULL default indicates using x
  if (is.null(.default))
    .default <- x

  # Standardize the lookup_table
  lookup_table <- standardize_lookup_table(lookup_table)

  # Check args (lookup table is checked separately)
  if (length(list(...)) != 0)
    stop("the ... args are reserved, but some were passed: ", names(list(...)))
  if (is.factor(x))
    stop("lookup() does not support factors")
  if (!(length(.default) %in% c(1, length(x))))
    stop("length(.default) must be 1 or length(x)")
  if (!is.atomic(x))
    stop("only atomic data types are supported")
  if (!test_equal_modes(x, .default, lookup_table$key, lookup_table$value))
    stop(paste0("x, .default, and both key and value of the lookup table ",
                "must all be of the same mode()\n The modes are: ",
                paste0(sapply(list(x, .default, lookup_table$key, lookup_table$value), mode), collapse=", ")))

  # Match and replace the found values from lookup_table
  ix <- match(x, lookup_table$key)
  x[!is.na(ix)] <- lookup_table$value[stats::na.exclude(ix)]

  # Replace unfound values with corresponding default values
  if (length(.default) == 1) {
    x[is.na(ix)] <- .default
  } else {
    x[is.na(ix)] <- .default[is.na(ix)]
  }

  # Return the result
  x
}

#' Construct lookup function based on a specific lookup table
#'
#' @description
#' The [lookuper()] function returns *a function* equivalent to the [lookup()]
#' function, except that instead of taking a lookup table as an argument, the
#' lookup table is embedded in the function itself.
#'
#' This can be very useful, in particular when using the lookup function as an
#' argument to other functions that expect a function which maps
#' `character`->`character` (or other data types), but do not offer a good way
#' to pass additional arguments to that function.
#'
#' @return The [lookuper()] function returns *a function* that takes vectors as
#'   its argument `x`, and returns either the corresponding values from the
#'   underlying lookup table, or the original values from x for those elements
#'   that are not found in the lookup table (or looks them up from the
#'   `default`).
#'
#' @examples
#' lookup_fruits <- lookuper(list(a = "Apple", b = "Banana", c = "Cherry"))
#' lookup_fruits(letters[1:5])
#' lookup_fruits_nomatch_na <-
#'   lookuper(list(a = "Apple", b = "Banana", c = "Cherry"), .default = NA)
#' lookup_fruits_nomatch_na(letters[1:5])
#'
#' @rdname lookup
#' @export
lookuper <- function(lookup_table, ..., .default = NULL) {

  # Check args, standardize the lookup_table
  lookup_table <- standardize_lookup_table(lookup_table)
  (length(list(...)) == 0) ||
    stop("the ... args are reserved, but some were passed: ", names(list(...)))
  is.null(.default) ||
    (is.atomic(.default) && (length(.default) == 1)) ||
    stop(".default must be NULL or a vector of length 1")

  # Return a function suitable for lookups
  result <- function(x) {
    lookup(x, lookup_table = lookup_table, .default = .default)
  }

  # Return result
  result
}


# Tests that all args either have the same mode, or include only NAs
test_equal_modes <- function(...) {
  l <- list(...)
  which.all.na <- vapply(l, \(x){all(is.na(x))}, logical(1))
  modes.not.all.na <- vapply(l, mode, character(1))[!which.all.na]
  return(length(unique(modes.not.all.na)) <= 1)
}

#' Helper function to standardize the `lookup_table`.
#'
#' Preprocessing the lookup table to convert it to a list can take some time, so
#' when possible, we want to do it only once. Therefore we offload it to a
#' helper function
#'
#' @param lookup_table The unstandardized lookup table (must still be one of the
#'   formats specified for the `lookup()` function).
#'
#' @return The lookup table as a list.
#'
#' @keywords internal
standardize_lookup_table <- function(lookup_table) {

  # Shim to convert the everything that is not a data.frame to one
  if (!is.data.frame(lookup_table))
    lookup_table <- as.data.frame(tibble::enframe(unlist(lookup_table)))

  # Check column names (if list or vector was passed, this should never fail)
  (sum(c("name", "key") %in% names(lookup_table)) == 1) ||
    stop("lookup_table colnames must include exactly one of 'key', 'name' (but not both)" )
  ("value" %in% names(lookup_table)) ||
    stop("lookup_table colnames must include 'value'" )

  # Rename 'name' to 'key' if needed
  if ("name" %in% names(lookup_table))
    names(lookup_table)[match("name", names(lookup_table))]  <- "key"

  # Return the standardized lookup_table
  lookup_table
}

