# Recode values using tilde syntax

Recode elements of a vector using a series of formulas (`lhs ~ rhs`)
passed via `...`. Each `lhs` is matched against elements of `x`, and the
corresponding `rhs` provides the new value.

This function is closely based on
[`dplyr::case_match()`](https://dplyr.tidyverse.org/reference/case_match.html)
with minimal changes to make it more intuitive for re-coding tasks. In
particular, rather than setting unmatched values to `NA` by default,
they remain unchanged `.default`, which itself defaults to `x`. The
output type can be controlled with `.ptype`. `.ptype` defaults to
`.default`, which means that type can be changed by setting `.default`
to either `NA` or to a value of the same type as the `rhs` formula
values. Incompatibility between the `rhs` values and the `.ptype`
results in a type error.

## Usage

``` r
recode_tilde(x, ..., .default = x, .ptype = NULL)
```

## Arguments

- x:

  A vector to recode.

- ...:

  Formulae specifying recoding rules, recoding from `lhs` to `rhs`.

- .default:

  Default value for unmatched inputs. Defaults to `x`.

- .ptype:

  Optional output type, defaults to `.default`. All values in `rhs` and
  `.default` must be mutually compatible with `.ptype`

## Value

A vector with recoded values.

## Examples

``` r
recode_tilde(letters, "a" ~ "first", "z" ~ "last")
#>  [1] "first" "b"     "c"     "d"     "e"     "f"     "g"     "h"     "i"    
#> [10] "j"     "k"     "l"     "m"     "n"     "o"     "p"     "q"     "r"    
#> [19] "s"     "t"     "u"     "v"     "w"     "x"     "y"     "last" 
recode_tilde(1:5, 1 ~ 10, 2 ~ 20)
#> [1] 10 20  3  4  5
# Recoding to different type requires explicit .default values
recode_tilde(1:4, 1 ~ "low", 2 ~ "medium", 3 ~ "high", .default = NA)
#> [1] "low"    "medium" "high"   NA      
```
