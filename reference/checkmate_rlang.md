# Assertion functions for scalars and atomic vectors

The
[checkmate](https://mllg.github.io/checkmate/reference/checkmate-package.html)
type checks, adapted to raise
[rlang](https://rlang.r-lib.org/reference/rlang-package.html) style
errors. Every function below takes the same arguments – `na.ok`,
`null.ok`, `attr.ok`, and `length` or `range` where they apply – rather
than the argument set of the
[checkmate](https://mllg.github.io/checkmate/reference/checkmate-package.html)
function behind it. The dots are reserved, so a name that does not match
raises rather than being quietly ignored.

They are meant to be cheap enough to leave at the top of any function:
the passing case is one call to the backing `check_*()`, a test, and a
return. Assembling a good message happens on the failing path, which
runs once and then stops.

|               |                   |                      |
|---------------|-------------------|----------------------|
| **R Type**    | **Scalar**        | **Vector**           |
| `logical`     | `chk_flag(x)`     | `chk_logical(x)`     |
| `character`   | `chk_string(x)`   | `chk_character(x)`   |
| `numeric`     | `chk_number(x)`   | `chk_numeric(x)`     |
| `integer`     | `chk_inumber(x)`⁴ | `chk_integer(x)`     |
| `double`      | `chk_dnumber(x)`⁴ | `chk_double(x)`      |
| `integerish`¹ | `chk_znumber(x)`  | `chk_integerish(x)`  |
| `naturalish`² | `chk_count(x)`    | `chk_naturalish(x)`⁴ |
| `factor`      | ³                 | `chk_factor(x)`      |
| `complex`     | ³                 | `chk_complex(x)`     |
| `raw`         | ³                 | `chk_raw(x)`         |
| `Date`        | `chk_day(x)`⁴     | `chk_date(x)`        |
| `POSIXct`     | `chk_instant(x)`⁴ | `chk_posixct(x)`     |
| Any type      | `chk_scalar(x)`   | `chk_atomic(x)`⁵     |

- ¹ `integerish` refers to functional integers (numbers that are very
  close to integer values), regardless of type (`integer` or `double` )

- ² `naturalish` refers to functional integers restricted to the natural
  numbers (zero and positive numbers)

- ³ No assertion functions are provided for scalar `factor`, `complex`,
  or `raw`

- ⁴ Not available in the
  [checkmate](https://mllg.github.io/checkmate/reference/checkmate-package.html)
  package

- ⁵ Note that
  [`checkmate::assert_vector()`](https://mllg.github.io/checkmate/reference/checkVector.html)
  accepts either a `vector` or a `list`, which is seldom what is wanted
  and is therefore *not* implemented here.

## Usage

``` r
chk_flag(x, ..., na.ok = FALSE, null.ok = FALSE, attr.ok = "names")

chk_string(
  x,
  ...,
  na.ok = FALSE,
  null.ok = FALSE,
  attr.ok = "names",
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

chk_inumber(
  x,
  ...,
  na.ok = FALSE,
  null.ok = FALSE,
  attr.ok = "names",
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

chk_znumber(
  x,
  ...,
  na.ok = FALSE,
  null.ok = FALSE,
  attr.ok = "names",
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

chk_day(
  x,
  ...,
  na.ok = FALSE,
  null.ok = FALSE,
  attr.ok = "names",
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

chk_scalar(x, ..., na.ok = FALSE, null.ok = FALSE, attr.ok = "names")

chk_logical(
  x,
  ...,
  na.ok = TRUE,
  null.ok = FALSE,
  attr.ok = "names",
  length = NULL
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

chk_numeric(
  x,
  ...,
  na.ok = TRUE,
  null.ok = FALSE,
  attr.ok = "names",
  length = NULL,
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

chk_double(
  x,
  ...,
  na.ok = TRUE,
  null.ok = FALSE,
  attr.ok = "names",
  length = NULL,
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

chk_date(
  x,
  ...,
  na.ok = TRUE,
  null.ok = FALSE,
  attr.ok = "names",
  length = NULL,
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

chk_atomic(x, ..., na.ok = TRUE, attr.ok = "names", length = NULL)

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

- range:

  Permitted range of values, under the same first/last rule as `length`.
  For the character types it constrains
  [`nchar()`](https://rdrr.io/r/base/nchar.html) of the elements
  instead, and for the date and time types the bounds are themselves
  `Date` or `POSIXct`. `NA` at an end means no bound there, and so does
  an infinite end wherever the type keeps that meaning.

- zero.ok:

  Is zero permitted?

- length:

  Permitted length. `NULL` for any length, a scalar for one exact
  length, or a vector whose first and last elements give the minimum and
  the maximum. Neither may be negative, and `NA` at an end, or `Inf` as
  the maximum, means no bound there.

## Value

The original object if the assertion passes.

## See also

[checkmate_rlang_other](https://torfason.github.io/zmisc/reference/checkmate_rlang_other.md)
for containers, classes, and arbitrary conditions.
