# Sample from a vector in a safe way

The `zample()` function duplicates the functionality of
[`sample()`](https://rdrr.io/r/base/sample.html), with the exception
that it does not attempt the (sometimes dangerous) user-friendliness of
switching the interpretation of the first element to a number if the
length of the vector is 1.

`zample()` *always* treats its first argument as a vector containing
elements that should be sampled, so code won't break in unexpected ways
when the input vector happens to be of length 1. The sample is taken by
subsetting `x`, so the class and attributes of `x` are preserved.

If the goal is indeed to sample from an interval between 1 and n, use
use `sample(n)` or `sample.int(n)` (but make sure to only pass vectors
of length one to those functions).

## Usage

``` r
zample(x, size = length(x), replace = FALSE, prob = NULL)
```

## Arguments

- x:

  The vector to sample from

- size:

  The number of elements to sample from `x` (defaults to `length(x)`)

- replace:

  Should elements be replaced after sampling (defaults to `false`)

- prob:

  A vector of probability weights (defaults to equal probabilities)

## Value

The resulting sample, of the same class as `x`

## Examples

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
set.seed(42); zample(7:11, size=13, replace=TRUE, prob=(5:1)^3)
#>  [1] 9 9 7 8 8 7 8 7 8 8 7 8 9
set.seed(42); sample(7:11, size=13, replace=TRUE, prob=(5:1)^3)
#>  [1] 9 9 7 8 8 7 8 7 8 8 7 8 9

# Of course, sampling more than the available elements without
# setting replace=TRUE will result in an error
set.seed(42); tryCatch(zample(7, size=2), error=wrap_error)
#> #E> cannot take a sample larger than the population
#> #E> when 'replace = FALSE'
```
