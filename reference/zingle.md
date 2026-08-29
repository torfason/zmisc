# Return the single (unique) value found in a vector

`zingle()` returns the only value present in a vector. If the vector
contains more than one distinct value, it throws an error. This is a
guard for aggregations where all values within a group should be
identical, but where you want that assumption checked rather than
assumed. Only values are compared. Names are ignored and the result is
unnamed, in line with other aggregation functions.

## Usage

``` r
zingle(
  x,
  ...,
  empty.ok = FALSE,
  na.ok.partial = FALSE,
  na.ok.all = FALSE,
  nan.ok.all = FALSE
)
```

## Arguments

- x:

  Vector of elements that should all be identical

- ...:

  Unused, reserved to force later arguments to be named

- empty.ok:

  Is an empty vector ok?

- na.ok.partial:

  Is a mix of `NA` and one distinct non-missing value ok?

- na.ok.all:

  Is a vector of only `NA` values ok?

- nan.ok.all:

  Is `NaN` ok as the returned value?

## Value

The single element in the vector, unnamed. For `na.ok.partial` the
non-missing value is returned.

## Examples

``` r
zingle(c("Alpha", "Alpha", "Alpha"))
#> [1] "Alpha"
zingle(c("Alpha", NA, "Alpha"), na.ok.partial = TRUE)
#> [1] "Alpha"
zingle(c(NA, NA), na.ok.all = TRUE)
#> [1] NA
zingle(c(NaN, NaN), nan.ok.all = TRUE)
#> [1] NaN
```
