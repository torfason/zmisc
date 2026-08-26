# Generate sequence in a safe way

The `zeq()` function creates an increasing integer sequence, but differs
from the standard one in that it will not silently generate a decreasing
sequence when the second argument is smaller than the first. If the
second argument is one smaller than the first it will generate an empty
sequence, if the difference is greater, the function will throw an
error.

## Usage

``` r
zeq(from, to)
```

## Arguments

- from:

  The lower bound of the sequence

- to:

  The higher bound of the sequence

## Value

A sequence ranging from `from` to `to`

## Examples

``` r
# For increasing sequences, zeq() and seq() are identical
zeq(11,15)
#> [1] 11 12 13 14 15
zeq(11,11)
#> [1] 11

# If second argument equals first-1, an empty sequence is returned
zeq(11,10)
#> numeric(0)

# If second argument is less than first-1, the function throws an error
tryCatch(zeq(11,9), error=wrap_error)
#> #E> to >= from - 1 is not TRUE
```
