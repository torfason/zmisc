# zmisc

## Vector Look-Ups and Safer Sampling

A collection of utility functions that facilitate looking up vector
values from a lookup table, annotate values in a table for clearer
viewing, and support a safer approach to vector sampling, sequence
generation, and aggregation. Also included is a family of argument
checks which return their input so that they compose nicely in a pipe.

## Installation

Install the released version from
[CRAN](https://cran.r-project.org/package=zmisc):

``` r

install.packages("zmisc")
```

Or the development version from
[GitHub](https://github.com/torfason/zmisc), using `pak`:

``` r

pak::pak("torfason/zmisc")
```

## Usage

``` r

library(zmisc)
```

## Quick and easy value lookups

[lookup()](https://torfason.github.io/zmisc/reference/lookup.html) looks
up values (such as variable names) in a lookup table that maps keys onto
values (such as variable labels). The table can be a two-column
`data.frame`, a named `vector`, or a `list`. Values that are not found
are returned unchanged, or replaced by `.default`.

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
```

[lookuper()](https://torfason.github.io/zmisc/reference/lookuper.html)
returns *a function* that does the same lookup, with the table embedded
in the function itself. That is useful wherever a
`character`-\>`character` function is expected but there is no good way
to pass the table along with it.

``` r

lookup_fruits <- lookuper(list(a = "Apple", b = "Banana", c = "Cherry"))
lookup_fruits(letters[1:5])
#> [1] "Apple"  "Banana" "Cherry" "d"      "e"
```

## Safer sampling, sequencing and aggregation

[zample()](https://torfason.github.io/zmisc/reference/zample.html),
[zeq()](https://torfason.github.io/zmisc/reference/zeq.html) and
[zingle()](https://torfason.github.io/zmisc/reference/zingle.html) make
code less likely to break in mysterious ways at awkward boundary
conditions.

[zample()](https://torfason.github.io/zmisc/reference/zample.html) is
[sample()](https://rdrr.io/r/base/sample.html) without the
user-friendliness of switching to `1:n` when the input happens to have
length 1.

``` r

# For vectors of length 2 or more, zample() and sample() are identical
set.seed(42); zample(7:11)
#> [1]  7 11 10  9  8
set.seed(42); sample(7:11)
#> [1]  7 11 10  9  8

# For length 1, sample() "magically" switches to sampling from 1:n
set.seed(42); zample(7)
#> [1] 7
set.seed(42); sample(7)
#> [1] 1 5 7 6 2 3 4
```

[zeq()](https://torfason.github.io/zmisc/reference/zeq.html) is an
increasing integer sequence that refuses to silently run backwards. A
second argument one below the first gives an empty sequence, anything
lower is an error.

``` r

zeq(11, 15)
#> [1] 11 12 13 14 15
zeq(11, 10)
#> integer(0)
tryCatch(zeq(11, 9), error = wrap_error)
#> #E> `to` must not be smaller than `from` - 1 (got
#> #E> from = 11, to = 9)
```

[zingle()](https://torfason.github.io/zmisc/reference/zingle.html)
returns the single distinct value in a vector, and errors if there is
more than one. This is useful in aggregations where all values in a
group should be identical, but where the assumption is worth checking.

``` r

zingle(c("Alpha", "Alpha", "Alpha"))
#> [1] "Alpha"
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

## Checks with rlang-style errors

The `chk_*()` functions check the type and shape of an argument and, on
failure, raise an [rlang](https://rlang.r-lib.org/)-style error naming
the argument as the caller wrote it. The checking itself is done by
[checkmate](https://mllg.github.io/checkmate/). Each function returns
its input, so a check can sit in the middle of a pipe, and each is cheap
enough on the passing path to leave at the top of any function.

``` r

# The check returns its input, so it composes in a pipe
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

There are checks for every atomic type in scalar and vector form, for
lists and composite objects, and for classes and arbitrary conditions.
See
[`vignette("chk")`](https://torfason.github.io/zmisc/articles/chk.md)
for the full set and the shared parameters.

## Getting a better view on variables

[notate()](https://torfason.github.io/zmisc/reference/notate.html)
annotates `factor` and `labelled` variables so that both values and
labels are visible at once, which neither printing nor
[View()](https://rdrr.io/r/utils/View.html) manages on its own.

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

## Other utilities

- [glue_vector()](https://torfason.github.io/zmisc/reference/glue_vector.html)
  applies a [glue()](https://glue.tidyverse.org/) template element-wise
  to a vector, for pipe-friendly interpolation outside a `data.frame`.
- [asciify()](https://torfason.github.io/zmisc/reference/asciify.html)
  transliterates accented characters to ASCII.
- [yencode()](https://torfason.github.io/zmisc/reference/yencode.html)
  encodes arbitrary strings into a restricted character set, and
  [ydecode()](https://torfason.github.io/zmisc/reference/yencode.html)
  reverses it.
