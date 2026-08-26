# Assert specific values and set memberships

Assert specific values and set memberships

## Usage

``` r
assert_choice(x, choices, ...)

assert_environment(x, ...)
```

## Arguments

- x:

  The variable to assert

- choices:

  A vector of values representing the which x must be an element of.

- ...:

  Additional parameters passed to corresponding checkmate functions
  [`checkmate::qtest()`](https://mllg.github.io/checkmate/reference/qassert.html),
  [`checkmate::check_flag()`](https://mllg.github.io/checkmate/reference/checkFlag.html),
  etc.

## Value

The original object if the assertion passes.
