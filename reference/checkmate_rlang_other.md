# Assertion functions for objects that are not atomic vectors

Assertions for containers and for properties that no type check covers,
with [rlang](https://rlang.r-lib.org/reference/rlang-package.html) style
error messages. See
[checkmate_rlang](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
for the scalar and vector types.

|                         |                                            |
|-------------------------|--------------------------------------------|
| **Function**            | **Passes when**                            |
| `chk_environment(x)`    | `x` is an environment                      |
| `chk_list(x)`           | `x` is a list, and carries no class        |
| `chk_data_frame(x)`     | `x` is a `data.frame` of sound structure   |
| `chk_data_table(x)`     | `x` is also a `data.table`                 |
| `chk_tibble(x)`         | `x` is also a `tbl_df`                     |
| `chk_class(x, classes)` | `x` inherits from every class in `classes` |
| `chk_true(x)`           | `x` is `TRUE`                              |

`chk_true()` is the catch-all: any property of any object that can be
written as a condition, at the cost of a message that can only report
that the condition was not met.

These take far fewer arguments than their
[checkmate](https://mllg.github.io/checkmate/reference/checkmate-package.html)
counterparts. The container checks carry no `attr.ok`, since a
`data.frame` is its class and its row names, and only `chk_list()` takes
a `length`.

## Usage

``` r
chk_environment(x, ..., null.ok = FALSE, contains = character())

chk_list(x, ..., null.ok = FALSE, length = NULL)

chk_data_frame(x, ..., null.ok = FALSE)

chk_data_table(x, ..., null.ok = FALSE)

chk_tibble(x, ..., null.ok = FALSE)

chk_class(x, classes, ..., null.ok = FALSE, ordered = FALSE)

chk_true(x, ..., na.ok = FALSE)
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

- length:

  Permitted length. `NULL` for any length, a scalar for one exact
  length, or a vector whose first and last elements give the minimum and
  the maximum. Neither may be negative, and `NA` at an end, or `Inf` as
  the maximum, means no bound there.

- classes:

  Character vector of class names `x` must inherit from.

- ordered:

  Must `classes` appear in that order at the head of `class(x)`?

- na.ok:

  Are missing values permitted?

## Value

The original object if the assertion passes.

## See also

[checkmate_rlang](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
for the scalar and vector types.
