# zmisc

## Vector Look-Ups and Safer Sampling

A collection of utility functions that facilitate looking up vector
values from a lookup table, annotate values in a table for clearer
viewing, and support a safer approach to vector sampling, sequence
generation, and aggregation. Also included is a family of argument
checks which return their input so that they compose nicely in a pipe.

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

### lookup: Get or set variable label of a labelled variable

Gets or sets the variable label (`label` attribute) of a labelled
vector. The getters/setters should be used rather than manipulating
attributes directly, since these functions perform checks to ensure that
the result, and the resulting labelled variable, are valid.

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

### zeq: Yet (another urlencode compatible) encoding scheme

#### Examples

``` r
```

### zingle:

#### Examples

``` r
```

## Checks with rlang-style errors

The `chk_*()` functions check the type and shape of an argument and, on
failure, raise an rlang-style error naming the argument as the caller
wrote it. Each returns its input, so a check composes in a pipe. See
[`vignette("chk")`](https://torfason.github.io/zmisc/articles/chk.md)
for the full set.

## Getting a better view on variables

The [notate()](https://torfason.github.io/zmisc/reference/notate.html)
function adds annotations to `factor` and `labelled` variables that make
it easier to see both values and labels/levels when using the
[View()](https://rdrr.io/r/utils/View.html) function

### notate:

#### Examples

``` r
```
