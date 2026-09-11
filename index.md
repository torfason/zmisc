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
#> [1] "Apple"  "Banana" "Cherry" "d"      "e"
lookup(letters[1:5], fruit_lookup_vector, .default = NA)
#> [1] "Apple"  "Banana" "Cherry" NA       NA

mtcars_lookup_data_frame <- data.frame(
  name = c("mpg", "hp", "wt"),
  value = c("Miles/(US) gallon", "Gross horsepower", "Weight (1000 lbs)"))
lookup(names(mtcars), mtcars_lookup_data_frame)
#>  [1] "Miles/(US) gallon" "cyl"               "disp"             
#>  [4] "Gross horsepower"  "drat"              "Weight (1000 lbs)"
#>  [7] "qsec"              "vs"                "am"               
#> [10] "gear"              "carb"

# A more complex example, with numeric and NA values
numeric_lookup_table <- data.frame(
  key = c(1:5, NA), value = c(sqrt(1:5), 99999))
lookup(c(0:6, NA), numeric_lookup_table)
#> [1]     0.000000     1.000000     1.414214     1.732051     2.000000
#> [6]     2.236068     6.000000 99999.000000
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
#> [1] "Apple"  "Banana" "Cherry" "d"      "e"

lookup_fruits_nomatch_na <-
  lookuper(list(a = "Apple", b = "Banana", c = "Cherry"), .default = NA)
lookup_fruits_nomatch_na(letters[1:5])
#> [1] "Apple"  "Banana" "Cherry" NA       NA
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
length of the vector is 1.
[`zample()`](https://torfason.github.io/zmisc/reference/zample.md)
*always* treats its first argument as a vector containing elements that
should be sampled, so your code won’t break in unexpected ways when the
input vector happens to be of length 1.

#### Examples

``` r

# For vectors of length 2 or more, zample() and sample() are identical
set.seed(42); zample(7:11)
#> [1]  7 11 10  9  8
set.seed(42); sample(7:11)
#> [1]  7 11 10  9  8

# For vectors of length 1, zample() will still sample from the vector,
# whereas sample() will "magically" switch to interpreting the input
# as a number n, and sampling from the vector 1:n.
set.seed(42); zample(7)
#> [1] 7
set.seed(42); sample(7)
#> [1] 1 5 7 6 2 3 4

# The other arguments work in the same way as for sample()
set.seed(42); zample(7:11, size = 13, replace = TRUE, prob = (5:1)^3)
#>  [1] 9 9 7 8 8 7 8 7 8 8 7 8 9

# Of course, sampling more than the available elements without
# setting replace=TRUE will result in an error
set.seed(42); tryCatch(zample(7, size = 2), error = wrap_error)
#> #E> cannot take a sample larger than the population
#> #E> when 'replace = FALSE'
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
zeq(11, 15)
#> [1] 11 12 13 14 15
zeq(11, 11)
#> [1] 11

# If second argument equals first-1, an empty sequence is returned
zeq(11, 10)
#> integer(0)

# If second argument is less than first-1, the function throws an error
tryCatch(zeq(11, 9), error = wrap_error)
#> #E> `to` must not be smaller than `from` - 1 (got
#> #E> from = 11, to = 9)
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
#> [1] "Alpha"

# If any elements differ, an error is thrown
tryCatch(zingle(c("Alpha", "Beta", "Alpha")), error = wrap_error)
#> #E> `x` must contain a single unique value, but found
#> #E> 2 distinct values.
```

``` r

if (require("dplyr", quietly = TRUE, warn.conflicts = FALSE)) {
  d <- data.frame(
    id    = c(1, 2, 1),
    name  = c("James", "Jack", "James"),
    fouls = c(3, 2, 4)
  )

  # If the data is of the correct format, all is good
  d |>
    group_by(id) |>
    summarise(name = zingle(name), total_fouls = sum(fouls))
}
#> # A tibble: 2 × 3
#>      id name  total_fouls
#>   <dbl> <chr>       <dbl>
#> 1     1 James           7
#> 2     2 Jack            2
```

``` r

if (require("dplyr", quietly = TRUE, warn.conflicts = FALSE)) {
  # If a name does not match its ID, we should get an error
  d[1, "name"] <- "Jammes"
  tryCatch({
    d |>
      group_by(id) |>
      summarise(name = zingle(name), total_fouls = sum(fouls))
  }, error = wrap_error)
}
#> #E> In argument: `name = zingle(name)`.
```

## Assertions with rlang-style errors

The `chk_*()` functions check the type and shape of an argument and, on
failure, raise an [rlang](https://rlang.r-lib.org/)-style error that
names the argument as the caller wrote it. The checking itself is done
by [checkmate](https://mllg.github.io/checkmate/). Each function returns
its input, so an assertion can sit in the middle of a pipe, and each is
cheap enough on the passing path to leave at the top of any function.

| R type | Scalar | Vector |
|----|----|----|
| `logical` | [`chk_flag()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md) | [`chk_logical()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md) |
| `character` | [`chk_string()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md) | [`chk_character()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md) |
| `numeric` | [`chk_number()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md) | [`chk_numeric()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md) |
| `integer` | [`chk_inumber()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md) | [`chk_integer()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md) |
| `double` | [`chk_dnumber()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md) | [`chk_double()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md) |
| integerish | [`chk_znumber()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md) | [`chk_integerish()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md) |
| naturalish | [`chk_count()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md) | [`chk_naturalish()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md) |
| `factor` |  | [`chk_factor()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md) |
| `complex` |  | [`chk_complex()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md) |
| `raw` |  | [`chk_raw()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md) |
| `Date` | [`chk_day()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md) | [`chk_date()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md) |
| `POSIXct` | [`chk_instant()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md) | [`chk_posixct()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md) |
| any type | [`chk_scalar()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md) | [`chk_atomic()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md) |

Every one of them takes the same small set of arguments rather than the
argument list of the [checkmate](https://mllg.github.io/checkmate/)
function behind it: `na.ok`, `null.ok`, `attr.ok`, and `length` or
`range` where they apply, plus `zero.ok` for the naturalish pair. The
dots are reserved, so a misspelled argument raises rather than being
quietly ignored.

#### Examples

``` r

# The assertion returns its input, so it composes in a pipe
c(2, 4, 6) |> chk_numeric(length = 3) |> sum()
#> [1] 12

# On failure, the error names the argument as the caller wrote it
my_mean <- function(x) {
  chk_numeric(x)
  sum(x) / length(x)
}
tryCatch(my_mean("seven"), error = wrap_error)
#> #E> Assertion on `x` failed: • Must be of type
#> #E> 'numeric', not 'character'
```

``` r

# One parameter set across the whole family
tryCatch(chk_character(c("a", NA), na.ok = FALSE), error = wrap_error)
#> #E> Assertion on `c("a", NA)` failed: • Contains
#> #E> missing values (element 2)
tryCatch(chk_integer(1:5, length = 3), error = wrap_error)
#> #E> Assertion on `1:5` failed: • Must have length 3,
#> #E> but has length 5
tryCatch(chk_numeric(c(1, 99), range = c(0, 10)), error = wrap_error)
#> #E> Assertion on `c(1, 99)` failed: • Element 2 is
#> #E> not <= 10
tryCatch(chk_count(0, zero.ok = FALSE), error = wrap_error)
#> #E> Assertion on `0` failed: • Must be >= 1

# attr.ok catches a vector that is not quite bare
labelled_ages <- structure(c(38L, 41L), label = "Age at interview")
tryCatch(chk_integer(labelled_ages), error = wrap_error)
#> #E> Assertion on `labelled_ages` failed: • Must not
#> #E> have attributes: label
```

Containers, classes and arbitrary conditions have their own smaller set,
taking `null.ok` and little else.
[`chk_true()`](https://torfason.github.io/zmisc/reference/checkmate_rlang_other.md)
is the catch-all, for any property that can be written as a condition.

``` r

tryCatch(chk_list(data.frame(a = 1)), error = wrap_error)
#> #E> Assertion on `data.frame(a = 1)` failed: • Must
#> #E> be of type 'list', not 'data.frame' • Must not
#> #E> have a class attribute, but has class
#> #E> "data.frame" • Must not have attributes:
#> #E> row.names
tryCatch(chk_class(1:3, "factor"), error = wrap_error)
#> #E> Assertion on `1:3` failed: • Must inherit from
#> #E> class 'factor', but has class 'integer'
tryCatch(chk_true(nrow(mtcars) > 100), error = wrap_error)
#> #E> Assertion on `nrow(mtcars) > 100` failed: • Must
#> #E> be TRUE
```

Two [rlang](https://rlang.r-lib.org/) assertions are re-exported under
the same naming convention.
[`chk_dots_empty()`](https://torfason.github.io/zmisc/reference/checkmate_rlang_dots.md)
fails if anything was passed through `...`, and
[`chk_match()`](https://torfason.github.io/zmisc/reference/checkmate_rlang_dots.md)
matches an argument against the values in its own default, returning the
match.

``` r

plot_kind <- function(kind = c("scatter", "line", "bar")) {
  kind <- chk_match(kind)
  kind
}
plot_kind()
#> [1] "scatter"
tryCatch(plot_kind("pie"), error = wrap_error)
#> #E> `kind` must be one of "scatter", "line", or
#> #E> "bar", not "pie".
```

## Getting a better view on variables

The [notate()](https://torfason.github.io/zmisc/reference/notate.html)
function adds annotations to `factor` and `labelled` variables that make
it easier to see both values and labels/levels when using the
[View()](https://rdrr.io/r/utils/View.html) function.

### notate: Embed factor levels and value labels in values

This function adds level/label information as an annotation to either
factors or `labelled` variables. This function is called
[`notate()`](https://torfason.github.io/zmisc/reference/notate.md)
rather than `annotate()` to avoid conflict with `ggplot2::annotate()`.
It is a generic that can operate either on individual vectors or on a
`data.frame`.

When printing `labelled` variables from a `tibble` in a console, both
the numeric value and the text label are shown, but no variable labels.
When using the [`View()`](https://rdrr.io/r/utils/View.html) function,
only variable labels are shown but no value labels. For factors, there
is no way to view the integer levels and values at the same time.

In order to allow the viewing of both variable and value labels at the
same time, this function converts both `factor` and `labelled` variables
to `character`, including both numeric levels (`labelled` values) and
character values (`labelled` labels) in the output.

#### Examples

``` r

d <- data.frame(
  chr = letters[1:4],
  fct = factor(c("alpha", "bravo", "chrly", "delta")),
  lbl = ll_labelled(c(1, 2, 3, NA),
                    labels = c(one = 1, two = 2),
                    label = "A labelled vector")
)
notate(d)
#>   chr       fct     lbl
#> 1   a [1] alpha [1] one
#> 2   b [2] bravo [2] two
#> 3   c [3] chrly     [3]
#> 4   d [4] delta    <NA>
# View(notate(d))
```
