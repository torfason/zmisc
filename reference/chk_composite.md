# Checks for lists and composite objects

Checks for composite objects. See
[chk_atomic](https://torfason.github.io/zmisc/reference/chk_atomic.md)
for scalar and vector types, and
[chk_other](https://torfason.github.io/zmisc/reference/chk_other.md) for
various other checks.

|                      |                                          |
|----------------------|------------------------------------------|
| **Function**         | **Passes when**                          |
| `chk_environment(x)` | `x` is an environment                    |
| `chk_list(x)`        | `x` is a list, and carries no class      |
| `chk_data_frame(x)`  | `x` is a `data.frame` of sound structure |
| `chk_data_table(x)`  | `x` is also a `data.table`               |
| `chk_tibble(x)`      | `x` is also a `tbl_df`                   |

## Usage

``` r
chk_environment(x, ..., null.ok = FALSE, contains = character())

chk_list(x, ..., null.ok = FALSE, attr.ok = "names", length = NULL)

chk_data_frame(x, ..., null.ok = FALSE)

chk_data_table(x, ..., null.ok = FALSE)

chk_tibble(x, ..., null.ok = FALSE)
```

## Arguments

- x:

  Object to check.

- ...:

  These dots are for future extensions and must be empty.

- null.ok:

  Is `NULL` permitted?

- contains:

  Character vector of names that must be bound in the environment.

- attr.ok:

  Which attributes `x` may carry beyond those intrinsic to its type: a
  character vector of permitted attribute names, `FALSE` for none at
  all, or `TRUE` for any. Applies to `chk_list()`.

- length:

  Permitted length. `NULL` for any length, a scalar for one exact
  length, or a vector whose first and last elements give the minimum and
  the maximum. Neither may be negative, and `NA` at an end, or `Inf` as
  the maximum, means no bound there.

## Value

The original object if the check passes.

## See also

[chk_atomic](https://torfason.github.io/zmisc/reference/chk_atomic.md)
for the scalar and vector types, and
[chk_other](https://torfason.github.io/zmisc/reference/chk_other.md) for
classes, conditions, and the
[rlang](https://rlang.r-lib.org/reference/rlang-package.html) aliases.

## Examples

``` r
chk_data_frame(mtcars)
chk_list(list(a = 1, b = 2), length = 2)

# chk_list() only accepts bare (unclassed) lists
tryCatch(chk_list(mtcars), error = wrap_error)
#> #E> Assertion on `mtcars` failed: • Must be
#> #E> of type 'list', not 'data.frame' • Must
#> #E> not have a class attribute, but has class
#> #E> "data.frame" • Must not have attributes:
#> #E> row.names
```
