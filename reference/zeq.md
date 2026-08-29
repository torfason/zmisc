# Generate sequence in a safe way

The `zeq()` function creates an increasing integer sequence, but differs
from the standard one in that it will not silently generate a decreasing
sequence when the second argument is smaller than the first. If the
second argument is one smaller than the first it will generate an empty
sequence, if the difference is greater, the function will throw an
error.

Both arguments must be a single `integerish` value (an `integer`, or a
`double` that is very close to one), and neither may be `NA`. Passing a
vector of length other than one is an error.

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

An `integer` sequence ranging from `from` to `to`, or an empty `integer`
vector if `to` equals `from - 1`.

## Examples

``` r
# For increasing sequences, zeq() and seq() are identical
zeq(11,15)
#> [1] 11 12 13 14 15
zeq(11,11)
#> [1] 11

# If second argument equals first-1, an empty sequence is returned
zeq(11,10)
#> integer(0)

# If second argument is less than first-1, the function throws an error
tryCatch(zeq(11,9), error=wrap_error)
#> #E> `to` must not be smaller than `from` - 1 (got
#> #E> from = 11, to = 9)

# Each bound must be a single whole number, so this errors as well
tryCatch(zeq(c(11,12),15), error=wrap_error)
#> #E> Must have length 1
```
