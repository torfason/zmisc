# Utility function to output an error

This function is used to capture errors, typically inside a
[`tryCatch()`](https://rdrr.io/r/base/conditions.html) statement and
output them in a clean and readable way. The function provides
line-wrapping, with a configurable width. When printing the error
message, it prefixes the text with "`#E> `" to make it easier to look
for the error.

## Usage

``` r
wrap_error(e, wrap = 50)
```

## Arguments

- e:

  The error to wrap.

- wrap:

  How many characters per line before wrapping.

## Value

The original error is returned invisibly.

## Examples

``` r
tryCatch(stop("This is an error"), error=wrap_error)
#> #E> This is an error
```
