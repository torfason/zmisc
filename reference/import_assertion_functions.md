# Generate imports for all assertions functions

Generates roxygen2 `@importFrom` statements for all assertion functions.

## Usage

``` r
import_assertion_functions(prefix = "assert_", width = 80)
```

## Arguments

- prefix:

  Prefix used to select exported names.

- width:

  Maximum output line width.

## Value

A character string containing `@importFrom` statements and `NULL`.
