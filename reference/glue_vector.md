# Glue interpolation vectors in pipes

Applies [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
to each element of a character vector using a template string, enabling
pipe-friendly, element-wise interpolation. Useful when the vector to
process is not encapsulated in a `data.frame` or other environment-like
object.

## Usage

``` r
glue_vector(
  .,
  template = "{.}",
  ...,
  .sep = "",
  .envir = parent.frame(),
  .open = "{",
  .close = "}",
  .na = "NA",
  .null = character(),
  .comment = "#",
  .literal = FALSE,
  .transformer = glue::identity_transformer,
  .trim = TRUE
)
```

## Arguments

- .:

  A character vector to be interpolated.

- template:

  A glue template string. Use `{.}` to refer to the default (unnamed)
  vector variable, or the names of any other variables accessible in the
  relevant environment. Variables are recycled using tidyverse recycling
  rules.

- ...:

  Reserved and should not be used.

- .sep, .envir, .open, .close, .na, .null, .comment, .literal,
  .transformer, .trim:

  Arguments passed on to
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html). Must
  be passed by name. See
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) for
  details.

## Value

A character vector with interpolated values. The length is determined by
tidyverse recycling rules for all referenced variables.

## Examples

``` r
  letters |> glue_vector("Letters include {.} and {LETTERS}")
#> Letters include a and A
#> Letters include b and B
#> Letters include c and C
#> Letters include d and D
#> Letters include e and E
#> Letters include f and F
#> Letters include g and G
#> Letters include h and H
#> Letters include i and I
#> Letters include j and J
#> Letters include k and K
#> Letters include l and L
#> Letters include m and M
#> Letters include n and N
#> Letters include o and O
#> Letters include p and P
#> Letters include q and Q
#> Letters include r and R
#> Letters include s and S
#> Letters include t and T
#> Letters include u and U
#> Letters include v and V
#> Letters include w and W
#> Letters include x and X
#> Letters include y and Y
#> Letters include z and Z
```
