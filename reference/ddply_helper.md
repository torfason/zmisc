# Apply a function to each column of a data.frame

Thin wrapper around [`lapply()`](https://rdrr.io/r/base/lapply.html)
that checks that the input is a table before applying the function to
each column, and converts the result back to a table afterwards. If the
`tibble` package is available and the input is a `tibble`, the result
will be a `tibble`; otherwise, it will be a plain `data.frame`.

## Usage

``` r
ddply_helper(d, fun)
```

## Arguments

- d:

  A `data.frame` or `tibble`.

- fun:

  A function to apply to each column of `d`.

## Value

A `data.frame` or `tibble` with the function applied to each column.

## Examples

``` r
df <- data.frame(
  col1 = c(1, 2, 3),
  col2 = c(4, 5, 6)
)
sum_fun <- function(x) sum(x)
result <- ddply_helper(df, sum_fun)
print(result)
#>   col1 col2
#> 1    6   15
```
