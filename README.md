zmisc
================

<!-- README.md is generated from README.Rmd -->

<!-- badges: start -->

[![CRAN
version](https://www.r-pkg.org/badges/version/zmisc)](https://CRAN.R-project.org/package=zmisc)
[![GitHub
version](https://img.shields.io/badge/Git-0.2.3.9004-success)](https://github.com/torfason/zmisc)
[![R-CMD-check](https://github.com/torfason/zmisc/workflows/R-CMD-check/badge.svg)](https://github.com/torfason/zmisc/actions)
[![R-CMD-check](https://github.com/torfason/zmisc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/torfason/zmisc/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Vector Look-Ups and Safer Sampling

A collection of utility functions that facilitate looking up vector
values from a lookup table, annotate values in at table for clearer
viewing, and support a safer approach to vector sampling, sequence
generation, and aggregation.

## Installation

You can install the released version of `zmisc` from
[CRAN](https://cran.r-project.org/package=zmisc) with:

``` r
install.packages("zmisc")
```

You can use `pak` to install the development version of `zmisc` from
[GitHub](https://github.com/torfason/zmisc) with:

``` r
pak::pak("torfason/zmisc")
```

## Usage

In order to use the package, you generally want to attach it first:

``` r
library(zmisc)
```

## Quick and easy value lookups

The functions
[lookup()](https://torfason.github.io/zmisc/reference/lookup.html) and
[lookuper()](https://torfason.github.io/zmisc/reference/lookuper.html)
are used to look up values from a lookup table, which can be supplied as
a `vector`, a `list`, or a `data.frame`. The functions are in some ways
similar to the Excel function `VLOOKUP()`, but are designed to work
smoothly in an R workflow, in particular within pipes.

### lookup: Lookup values from a lookup table

The [lookup()](https://torfason.github.io/zmisc/reference/lookup.html)
function implements lookup of values (such as variable names) from a
lookup table which maps keys onto values (such as variable labels or
descriptions).

The lookup table can be in the form of a two-column `data.frame`, in the
form of a named `vector`, or in the form of a `list`. If the table is in
the form of a `data.frame`, the key column should be named either `key`
or `name`, and the value column should be named `value` (for the value).
If the lookup table is in the form of a named `vector` or `list`, the
names are used as the key, and the returned value is taken from the
values in the vector or list.

The underlying lookup is done using `base::match()`, and all atomic data
types except `factor` are supported. Factors are omitted due to the
ambiguity in what should be looked up (the values or the levels). It is
important that `x`, `.default` and the columns of `lookup_table` are all
of the same type (specifically of the same `base::mode()`). If the
lookup table is specified as a `vector` or `list`, only the `character`
variables are supported, because `name(lookup_table)` is always of mode
`character`.

Original values are returned if they are not found in the lookup table.
Alternatively, a `.default` can be specified for values that are not
found. Note that it is possible to specify `NA` as one of the keys to
look up NA values (only when using a `data.frame` as lookup table).

Any names or attributes of x are preserved.

#### Examples

``` r
fruit_lookup_vector <- c(a = "Apple", b = "Banana", c = "Cherry")
lookup(letters[1:5], fruit_lookup_vector)
lookup(letters[1:5], fruit_lookup_vector, .default = NA)

mtcars_lookup_data_frame <- data.frame(
  name = c("mpg", "hp", "wt"),
  value = c("Miles/(US) gallon", "Gross horsepower", "Weight (1000 lbs)"))
lookup(names(mtcars), mtcars_lookup_data_frame)

# A more complex example, with numeric and NA values
numeric_lookup_table <- data.frame(
  key = c(1:5, NA), value = c(sqrt(1:5), 99999))
lookup(c(0:6, NA), numeric_lookup_table)
```

### lookuper: Construct lookup function based on a specific lookup table

The
[lookuper()](https://torfason.github.io/zmisc/reference/lookuper.html)
function returns *a function* equivalent to the
[lookup()](https://torfason.github.io/zmisc/reference/lookup.html)
function, except that instead of taking a lookup table as an argument,
the lookup table is embedded in the function itself.

This can be very useful, in particular when using the lookup function as
an argument to other functions that expect a function which maps
`character`-\>`character` (or other data types), but do not offer a good
way to pass additional arguments to that function.

#### Examples

``` r
lookup_fruits <- lookuper(list(a = "Apple", b = "Banana", c = "Cherry"))
lookup_fruits(letters[1:5])
lookup_fruits_nomatch_na <-
  lookuper(list(a = "Apple", b = "Banana", c = "Cherry"), .default = NA)
lookup_fruits_nomatch_na(letters[1:5])
```

## Safer sampling, sequencing and aggregation

The functions
[zample()](https://torfason.github.io/zmisc/reference/zample.html),
[zeq()](https://torfason.github.io/zmisc/reference/zeq.html), and
[zingle()](https://torfason.github.io/zmisc/reference/zingle.html) are
intended to make your code less likely to break in mysterious ways when
you encounter unexpected boundary conditions. The
[zample()](https://torfason.github.io/zmisc/reference/zample.html) and
[zeq()](https://torfason.github.io/zmisc/reference/zeq.html) are almost
identical to the [sample()](https://rdrr.io/r/base/sample.html) and
[seq()](https://rdrr.io/r/base/seq.html) functions, but a bit safer.

### zample: Sample from a vector in a safe way

The [zample()](https://torfason.github.io/zmisc/reference/zample.html)
function duplicates the functionality of
[sample()](https://rdrr.io/r/base/sample.html), with the exception that
it does not attempt the (sometimes dangerous) user-friendliness of
switching the interpretation of the first element to a number if the
length of the vector is 1. `zample()` *always* treats its first argument
as a vector containing elements that should be sampled, so your code
won’t break in unexpected ways when the input vector happens to be of
length 1.

#### Examples

``` r
# For vectors of length 2 or more, zample() and sample() are identical
set.seed(42); zample(7:11)
set.seed(42); sample(7:11)

# For vectors of length 1, zample() will still sample from the vector,
# whereas sample() will "magically" switch to interpreting the input
# as a number n, and sampling from the vector 1:n.
set.seed(42); zample(7)
set.seed(42); sample(7)

# The other arguments work in the same way as for sample()
set.seed(42); zample(7:11, size=13, replace=TRUE, prob=(5:1)^3)
set.seed(42); sample(7:11, size=13, replace=TRUE, prob=(5:1)^3)

# Of course, sampling more than the available elements without
# setting replace=TRUE will result in an error
set.seed(42); tryCatch(zample(7, size=2), error=wrap_error)
```

### zeq: Generate sequence in a safe way

The [zeq()](https://torfason.github.io/zmisc/reference/zeq.html)
function creates an increasing integer sequence, but differs from the
standard one in that it will not silently generate a decreasing sequence
when the second argument is smaller than the first. If the second
argument is one smaller than the first it will generate an empty
sequence, if the difference is greater, the function will throw an
error.

#### Examples

``` r
# For increasing sequences, zeq() and seq() are identical
zeq(11,15)
zeq(11,11)

# If second argument equals first-1, an empty sequence is returned
zeq(11,10)

# If second argument is less than first-1, the function throws an error
tryCatch(zeq(11,9), error=wrap_error)
```

### zingle: Return the single (unique) value found in a vector

The [zingle()](https://torfason.github.io/zmisc/reference/zingle.html)
function returns the first element in a vector, but only if all the
other elements are identical to the first one (the vector only has a
`zingle` value). If the elements are not all identical, it throws an
error. The vector must contain at least one non-`NA` value, or the
function errors out as well. This is especially useful in aggregations,
when all values in a given group should be identical, but you want to
make sure.

#### Examples

``` r
# If all elements are identical, all is good.
# The value of the element is returned.
zingle(c("Alpha", "Alpha", "Alpha"))

# If any elements differ, an error is thrown
tryCatch(zingle(c("Alpha", "Beta", "Alpha")), error=wrap_error)

if (require("dplyr", quietly=TRUE, warn.conflicts=FALSE)) {
  d <- tibble::tribble(
    ~id, ~name, ~fouls,
    1, "James", 3,
    2, "Jack",  2,
    1, "James", 4
  )

  # If the data is of the correct format, all is good
  d %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(name=zingle(name), total_fouls=sum(fouls))
 }

if (require("dplyr", quietly=TRUE, warn.conflicts=FALSE)) {
  # If a name does not match its ID, we should get an error
  d[1,"name"] <- "Jammes"
  tryCatch({
    d %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(name=zingle(name), total_fouls=sum(fouls))
  }, error=wrap_error)
}
```

## Getting a better view on variables

The [notate()](https://torfason.github.io/zmisc/reference/zingle.html)
function adds annotations to `factor` and `labelled` variables that make
it easier to see both values and labels/levels when using the
[View()](https://rdrr.io/r/utils/View.html) function

### notate: Embed factor levels and value labels in values.

This function adds level/label information as an annotation to either
factors or `labelled` variables. This function is called `notate()`
rather than `annotate()` to avoid conflict with `ggplot2::annotate()`.
It is a generic that can operate either on individual vectors or on a
`data.frame`.

When printing `labelled` variables from a `tibble` in a console, both
the numeric value and the text label are shown, but no variable labels.
When using the `View()` function, only variable labels are shown but no
value labels. For factors, there is no way to view the integer levels
and values at the same time.

In order to allow the viewing of both variable and value labels at the
same time, this function converts both `factor` and `labelled` variables
to `character`, including both numeric levels (`labelled` values) and
character values (`labelled` labels) in the output.

#### Examples

``` r
if (getRversion() >= "4") {
  d <- data.frame(
    chr = letters[1:4],
    fct = factor(c("alpha", "bravo", "chrly", "delta")),
    lbl = ll_labelled(c(1, 2, 3, NA),
                      labels = c(one=1, two=2),
                      label = "A labelled vector")
  )
  dn <- notate(d)
  dn
  # View(dn)
}
```
