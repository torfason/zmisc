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

### lookup: Get or set value labels of a labelled variable

Gets or sets the value labels (`labels` attribute) of a labelled vector.
The getters/setters should be used rather than manipulating attributes
directly, since these functions perform checks to ensure that the
result, and the resulting labelled variable, are valid.

#### Examples

``` r
```

### lookuper:

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

### zeq:

#### Examples

``` r
```

### zingle:

#### Examples

``` r
```

## Getting a better view on variables

The [notate()](https://torfason.github.io/zmisc/reference/zingle.html)
function adds annotations to `factor` and `labelled` variables that make
it easier to see both values and labels/levels when using the
[View()](https://rdrr.io/r/utils/View.html) function

### notate: Lookup values from a lookup table

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

The underlying lookup is done using
[`base::match()`](https://rdrr.io/r/base/match.html), and all atomic
data types except `factor` are supported. Factors are omitted due to the
ambiguity in what should be looked up (the values or the levels). It is
important that `x`, `.default` and the columns of `lookup_table` are all
of the same type (specifically of the same
[`base::mode()`](https://rdrr.io/r/base/mode.html)). If the lookup table
is specified as a `vector` or `list`, only the `character` variables are
supported, because `name(lookup_table)` is always of mode `character`.

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
