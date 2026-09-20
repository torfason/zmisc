# Checks for scalars and atomic vectors

Validity checks for atomic vectors. Each function below takes the same
arguments – `na.ok`, `null.ok`, `attr.ok`, and `length` or `range` where
they apply.

They are meant to be cheap enough to leave at the top of any function, a
passing value with default arguments tends to take under a microsecond
on a modern computer; with extra specification arguments it can take up
to ten microseconds. Assembling a good message happens on the failing
path, which runs once and then stops.

|               |                   |                     |
|---------------|-------------------|---------------------|
| **R Type**    | **Scalar**        | **Vector**          |
| Any type      | `chk_scalar(x)`   | `chk_atomic(x)`     |
| `logical`     | `chk_flag(x)`     | `chk_logical(x)`    |
| `character`   | `chk_string(x)`   | `chk_character(x)`  |
| `numeric`     | `chk_number(x)`   | `chk_numeric(x)`    |
| `integer`     | `chk_inumber(x)`⁴ | `chk_integer(x)`    |
| `double`      | `chk_dnumber(x)`⁴ | `chk_double(x)`     |
| `integerish`¹ | `chk_znumber(x)`  | `chk_integerish(x)` |
| `naturalish`² | `chk_count(x)`    | `chk_naturalish(x)` |
| `factor`      | ³                 | `chk_factor(x)`     |
| `complex`     | ³                 | `chk_complex(x)`    |
| `raw`         | ³                 | `chk_raw(x)`        |
| `Date`        | `chk_day(x)`      | `chk_date(x)`       |
| `POSIXct`     | `chk_instant(x)`  | `chk_posixct(x)`    |

- ¹ `integerish` refers to functional integers (numbers that are very
  close to integer values), regardless of type (`integer` or `double`)

- ² `naturalish` refers to functional integers restricted to the natural
  numbers (zero and positive numbers)

- ³ No check functions are provided for scalar `factor`, `complex`, or
  `raw`

## Usage

``` r
chk_flag(x, ..., na.ok = FALSE, null.ok = FALSE, attr.ok = "names")

chk_logical(
  x,
  ...,
  na.ok = TRUE,
  null.ok = FALSE,
  attr.ok = "names",
  length = NULL
)

chk_string(
  x,
  ...,
  na.ok = FALSE,
  null.ok = FALSE,
  attr.ok = "names",
  range = NULL
)

chk_character(
  x,
  ...,
  na.ok = TRUE,
  null.ok = FALSE,
  attr.ok = "names",
  length = NULL,
  range = NULL
)

chk_number(
  x,
  ...,
  na.ok = FALSE,
  null.ok = FALSE,
  attr.ok = "names",
  range = NULL
)

chk_numeric(
  x,
  ...,
  na.ok = TRUE,
  null.ok = FALSE,
  attr.ok = "names",
  length = NULL,
  range = NULL
)

chk_inumber(
  x,
  ...,
  na.ok = FALSE,
  null.ok = FALSE,
  attr.ok = "names",
  range = NULL
)

chk_integer(
  x,
  ...,
  na.ok = TRUE,
  null.ok = FALSE,
  attr.ok = "names",
  length = NULL,
  range = NULL
)

chk_dnumber(
  x,
  ...,
  na.ok = FALSE,
  null.ok = FALSE,
  attr.ok = "names",
  range = NULL
)

chk_double(
  x,
  ...,
  na.ok = TRUE,
  null.ok = FALSE,
  attr.ok = "names",
  length = NULL,
  range = NULL
)

chk_znumber(
  x,
  ...,
  na.ok = FALSE,
  null.ok = FALSE,
  attr.ok = "names",
  range = NULL
)

chk_integerish(
  x,
  ...,
  na.ok = TRUE,
  null.ok = FALSE,
  attr.ok = "names",
  length = NULL,
  range = NULL
)

chk_count(
  x,
  ...,
  na.ok = FALSE,
  zero.ok = TRUE,
  null.ok = FALSE,
  attr.ok = "names"
)

chk_naturalish(
  x,
  ...,
  na.ok = TRUE,
  zero.ok = TRUE,
  null.ok = FALSE,
  attr.ok = "names",
  length = NULL,
  range = NULL
)

chk_factor(
  x,
  ...,
  na.ok = TRUE,
  null.ok = FALSE,
  attr.ok = "names",
  length = NULL
)

chk_complex(
  x,
  ...,
  na.ok = TRUE,
  null.ok = FALSE,
  attr.ok = "names",
  length = NULL
)

chk_raw(x, ..., null.ok = FALSE, attr.ok = "names", length = NULL)

chk_day(
  x,
  ...,
  na.ok = FALSE,
  null.ok = FALSE,
  attr.ok = "names",
  range = NULL
)

chk_date(
  x,
  ...,
  na.ok = TRUE,
  null.ok = FALSE,
  attr.ok = "names",
  length = NULL,
  range = NULL
)

chk_instant(
  x,
  ...,
  na.ok = FALSE,
  null.ok = FALSE,
  attr.ok = "names",
  range = NULL
)

chk_posixct(
  x,
  ...,
  na.ok = TRUE,
  null.ok = FALSE,
  attr.ok = "names",
  length = NULL,
  range = NULL
)

chk_scalar(x, ..., na.ok = FALSE, null.ok = FALSE, attr.ok = "names")

chk_atomic(x, ..., na.ok = TRUE, attr.ok = "names", length = NULL)
```

## Arguments

- x:

  Object to check.

- ...:

  These dots are for future extensions and must be empty.

- na.ok:

  Are missing values permitted?

- null.ok:

  Is `NULL` permitted?

- attr.ok:

  Which attributes `x` may carry beyond those intrinsic to its type: a
  character vector of permitted attribute names, `FALSE` for none at
  all, or `TRUE` for any.

- length:

  Permitted length. `NULL` for any length, a scalar for one exact
  length, or a vector whose first and last elements give the minimum and
  the maximum. Neither may be negative, and `NA` at an end, or `Inf` as
  the maximum, means no bound there.

- range:

  Permitted range of values, under the same first/last rule as `length`.
  For the character types it constrains
  [`nchar()`](https://rdrr.io/r/base/nchar.html) of the elements
  instead, and for the date and time types the bounds are themselves
  `Date` or `POSIXct`. `NA` at an end means no bound there, and so does
  an infinite end wherever the type keeps that meaning.

- zero.ok:

  Is zero permitted?

## Value

The original object if the check passes.

## See also

[chk_composite](https://torfason.github.io/zmisc/reference/chk_composite.md)
for lists and list-based objects, and
[chk_other](https://torfason.github.io/zmisc/reference/chk_other.md) for
classes, conditions, and various other checks

## Examples

``` r
# A check returns its input (invisibly), so it composes in a pipe
c(2, 4, 6) |> chk_numeric(length = 3) |> sum()
#> [1] 12

# One parameter set across the whole family
chk_string("abc", range = c(1, 3))
chk_integer(1:5, length = c(1, 10))

# On failure, the error names the argument as the caller wrote it
my_mean <- function(x) {
  chk_numeric(x)
  sum(x) / length(x)
}
tryCatch(my_mean("seven"), error = wrap_error)
#> #E> Assertion on `x` failed: • Must be of
#> #E> type 'numeric', not 'character'
```
