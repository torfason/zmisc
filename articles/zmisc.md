# zmisc

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

### lookup:

#### Examples

``` r
```

### lookuper: Get the character representation of a labelled variable

Returns a character representation of a labelled variable, using the
value labels to look up the label for a given value.

The default behavior of this function is similar to
\[labelled::to_character()\]. The options, however, are slightly
different. Most importantly, instead of specifying `NA` handling using
parameters, the function relies on the `default` parameter to determine
what happens for unlabelled variables, allowing users to specify
including the original values of `x` instead of the labels, returning
`NA`, or returning a specific string value. Also, the default behavior
is to drop any variable label attribute, in line with the default
\[as.character()\] method.

#### Examples

``` r
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

### zample:

#### Examples

``` r
```

### zeq: Recode values using tilde syntax

Recode elements of a vector using a series of formulas (`lhs ~ rhs`)
passed via `...`. Each `lhs` is matched against elements of `x`, and the
corresponding `rhs` provides the new value.

This function is closely based on \[dplyr::case_match()\] with minimal
changes to make it more intuitive for re-coding tasks. In particular,
rather than setting unmatched values to `NA` by default, they remain
unchanged `.default`, which itself defaults to `x`. The output type can
be controlled with `.ptype`. `.ptype` defaults to `.default`, which
means that type can be changed by setting `.default` to either `NA` or
to a value of the same type as the `rhs` formula values. Incompatibility
between the `rhs` values and the `.ptype` results in a type error.

#### Examples

``` r

recode_tilde(letters, "a" ~ "first", "z" ~ "last")
recode_tilde(1:5, 1 ~ 10, 2 ~ 20)
# Recoding to different type requires explicit .default values
recode_tilde(1:4, 1 ~ "low", 2 ~ "medium", 3 ~ "high", .default = NA)
```

### zingle: Sample from a vector in a safe way

The [zample()](https://torfason.github.io/zmisc/reference/zample.html)
function duplicates the functionality of
[sample()](https://rdrr.io/r/base/sample.html), with the exception that
it does not attempt the (sometimes dangerous) user-friendliness of
switching the interpretation of the first element to a number if the
length of the vector is 1.
[`zample()`](https://torfason.github.io/zmisc/reference/zample.md)
*always* treats its first argument as a vector containing elements that
should be sampled, so your code won’t break in unexpected ways when the
input vector happens to be of length 1.

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

## Getting a better view on variables

The [notate()](https://torfason.github.io/zmisc/reference/zingle.html)
function adds annotations to `factor` and `labelled` variables that make
it easier to see both values and labels/levels when using the
[View()](https://rdrr.io/r/utils/View.html) function

### notate: Construct lookup function based on a specific lookup table

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
