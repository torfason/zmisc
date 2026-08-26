# Helper function to standardize the `lookup_table`.

Preprocessing the lookup table to convert it to a list can take some
time, so when possible, we want to do it only once. Therefore we offload
it to a helper function

## Usage

``` r
standardize_lookup_table(lookup_table)
```

## Arguments

- lookup_table:

  The unstandardized lookup table (must still be one of the formats
  specified for the
  [`lookup()`](https://torfason.github.io/zmisc/reference/lookup.md)
  function).

## Value

The lookup table as a list.
