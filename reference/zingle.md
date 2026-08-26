# Return the single (unique) value found in a vector

The `zingle()` function returns the first element in a vector, but only
if all the other elements are identical to the first one (the vector
only has a `zingle` value). If the elements are not all identical, it
throws an error. The vector must contain at least one non-`NA` value, or
the function errors out as well. This is especially useful in
aggregations, when all values in a given group should be identical, but
you want to make sure.

## Usage

``` r
zingle(x, na.rm = FALSE)
```

## Arguments

- x:

  Vector of elements that should all be identical

- na.rm:

  Should `NA` elements be removed prior to comparison

## Value

The `zingle` element in the vector

## Details

Optionally takes a `na.rm` parameter, similarly to sum, mean and other
aggregate functions. If `TRUE`, `NA` values will be removed prior to
comparing the elements, so the function will accept input values that
contain a combination of the single value and any `NA` values (but at
least one non-`NA` value is required).

Only values are tested for equality. Any names are simply ignored, and
the result is an unnamed value. This is in line with how other
aggregation functions handle names.

## Examples

``` r
# If all elements are identical, all is good.
# The value of the element is returned.
zingle(c("Alpha", "Alpha", "Alpha"))
#> [1] "Alpha"

# If any elements differ, an error is thrown
tryCatch(zingle(c("Alpha", "Beta", "Alpha")), error=wrap_error)
#> #E> all(x[1] == x) is not TRUE

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
#> # A tibble: 2 × 3
#>      id name  total_fouls
#>   <dbl> <chr>       <dbl>
#> 1     1 James           7
#> 2     2 Jack            2

if (require("dplyr", quietly=TRUE, warn.conflicts=FALSE)) {
  # If a name does not match its ID, we should get an error
  d[1,"name"] <- "Jammes"
  tryCatch({
    d %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(name=zingle(name), total_fouls=sum(fouls))
  }, error=wrap_error)
}
#> #E> In argument: `name = zingle(name)`.
```
