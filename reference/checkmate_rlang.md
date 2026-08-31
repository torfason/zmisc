# Assertion functions adapted for rlang output

Most common
[checkmate](https://mllg.github.io/checkmate/reference/checkmate-package.html)
functions, adapted to output
[rlang](https://rlang.r-lib.org/reference/rlang-package.html) style
error messages on failed assertions. The actual checking is done by
[`checkmate::qtest()`](https://mllg.github.io/checkmate/reference/qassert.html),
[`checkmate::check_flag()`](https://mllg.github.io/checkmate/reference/checkFlag.html)
and related functions.

### Performance

These functions are meant to be cheap enough to leave in place at the
top of any function, so the passing case is kept to the smallest amount
of work that will do: a single call to the underlying `check_*()`
function, a test of the result, and a return. Anything more expensive
belongs on the failing path, which runs once and then stops, and where
the cost of assembling a better message does not matter.

### Scalars and (atomic) vectors

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

### Composite Objects

|  |  |  |
|----|----|----|
| **R Type** | **Function** | **Note** |
| `environment` | `chk_environment(x)` | `is.environment(x)` |
| `list` | `chk_list(x)` | `is.list(x)` *and* x is unclassed. |
| `data.frame` | `chk_data_frame(x)` | `is.list(x)`, with class `data.frame` and correct structure. |
| `data.table` | `chk_data_table(x)`⁴ | `data.table::is.data.table(x)` *and* x is a `data.frame`. |
| `tibble` (`tbl_df`) | `chk_tibble(x)` | `tibble::is_tibble(x)` *and* x is a `data.frame`. |

## Usage

``` r
qassert(x, ...)

chk_flag(x, na.ok = FALSE, null.ok = FALSE, dim.ok = FALSE, class.ok = FALSE)

chk_string(
  x,
  na.ok = FALSE,
  n.chars = NULL,
  min.chars = NULL,
  max.chars = NULL,
  pattern = NULL,
  fixed = NULL,
  ignore.case = FALSE,
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_number(
  x,
  na.ok = FALSE,
  lower = -Inf,
  upper = Inf,
  finite = FALSE,
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_inumber(
  x,
  na.ok = FALSE,
  lower = -Inf,
  upper = Inf,
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_dnumber(
  x,
  na.ok = FALSE,
  lower = -Inf,
  upper = Inf,
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_znumber(
  x,
  na.ok = FALSE,
  lower = -Inf,
  upper = Inf,
  tol = sqrt(.Machine$double.eps),
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_count(
  x,
  na.ok = FALSE,
  positive = FALSE,
  tol = sqrt(.Machine$double.eps),
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_day(x, ...)

chk_instant(x, ...)

chk_scalar(x, na.ok = FALSE, null.ok = FALSE, dim.ok = FALSE, class.ok = FALSE)

chk_logical(
  x,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  names = NULL,
  typed.missing = FALSE,
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_character(
  x,
  n.chars = NULL,
  min.chars = NULL,
  max.chars = NULL,
  pattern = NULL,
  fixed = NULL,
  ignore.case = FALSE,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  sorted = FALSE,
  names = NULL,
  typed.missing = FALSE,
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_numeric(
  x,
  lower = -Inf,
  upper = Inf,
  finite = FALSE,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  sorted = FALSE,
  names = NULL,
  typed.missing = FALSE,
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_integer(
  x,
  lower = -Inf,
  upper = Inf,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  sorted = FALSE,
  names = NULL,
  typed.missing = FALSE,
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_double(
  x,
  lower = -Inf,
  upper = Inf,
  finite = FALSE,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  sorted = FALSE,
  names = NULL,
  typed.missing = FALSE,
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_integerish(
  x,
  tol = sqrt(.Machine$double.eps),
  lower = -Inf,
  upper = Inf,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  sorted = FALSE,
  names = NULL,
  typed.missing = FALSE,
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_naturalish(
  x,
  tol = sqrt(.Machine$double.eps),
  upper = Inf,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  sorted = FALSE,
  names = NULL,
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_factor(x, ...)

chk_complex(
  x,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  names = NULL,
  typed.missing = FALSE,
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_raw(
  x,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  names = NULL,
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_date(x, ...)

chk_posixct(x, ...)

chk_atomic(
  x,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  names = NULL,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_environment(x, ...)

chk_list(x, ...)

chk_data_frame(x, ...)

chk_data_table(x, ...)

chk_tibble(x, ...)

chk_class(x, ...)

chk_choice(x, choices, ...)

chk_flag(x, na.ok = FALSE, null.ok = FALSE, dim.ok = FALSE, class.ok = FALSE)

chk_logical(
  x,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  names = NULL,
  typed.missing = FALSE,
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_string(
  x,
  na.ok = FALSE,
  n.chars = NULL,
  min.chars = NULL,
  max.chars = NULL,
  pattern = NULL,
  fixed = NULL,
  ignore.case = FALSE,
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_character(
  x,
  n.chars = NULL,
  min.chars = NULL,
  max.chars = NULL,
  pattern = NULL,
  fixed = NULL,
  ignore.case = FALSE,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  sorted = FALSE,
  names = NULL,
  typed.missing = FALSE,
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_number(
  x,
  na.ok = FALSE,
  lower = -Inf,
  upper = Inf,
  finite = FALSE,
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_numeric(
  x,
  lower = -Inf,
  upper = Inf,
  finite = FALSE,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  sorted = FALSE,
  names = NULL,
  typed.missing = FALSE,
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_inumber(
  x,
  na.ok = FALSE,
  lower = -Inf,
  upper = Inf,
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_integer(
  x,
  lower = -Inf,
  upper = Inf,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  sorted = FALSE,
  names = NULL,
  typed.missing = FALSE,
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_dnumber(
  x,
  na.ok = FALSE,
  lower = -Inf,
  upper = Inf,
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_double(
  x,
  lower = -Inf,
  upper = Inf,
  finite = FALSE,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  sorted = FALSE,
  names = NULL,
  typed.missing = FALSE,
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_znumber(
  x,
  na.ok = FALSE,
  lower = -Inf,
  upper = Inf,
  tol = sqrt(.Machine$double.eps),
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_integerish(
  x,
  tol = sqrt(.Machine$double.eps),
  lower = -Inf,
  upper = Inf,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  sorted = FALSE,
  names = NULL,
  typed.missing = FALSE,
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_count(
  x,
  na.ok = FALSE,
  positive = FALSE,
  tol = sqrt(.Machine$double.eps),
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_naturalish(
  x,
  tol = sqrt(.Machine$double.eps),
  upper = Inf,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  sorted = FALSE,
  names = NULL,
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_complex(
  x,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  names = NULL,
  typed.missing = FALSE,
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_raw(
  x,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  names = NULL,
  null.ok = FALSE,
  dim.ok = FALSE,
  class.ok = FALSE
)

chk_scalar(x, na.ok = FALSE, null.ok = FALSE, dim.ok = FALSE, class.ok = FALSE)

chk_atomic(
  x,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  names = NULL,
  dim.ok = FALSE,
  class.ok = FALSE
)
```

## Arguments

- x:

  Object to check.

- ...:

  Additional parameters passed to corresponding
  [checkmate](https://mllg.github.io/checkmate/reference/checkmate-package.html)
  functions
  [`checkmate::qtest()`](https://mllg.github.io/checkmate/reference/qassert.html),
  [`checkmate::check_flag()`](https://mllg.github.io/checkmate/reference/checkFlag.html),
  etc.

- na.ok:

  Passed to the backing `check_*()` function as `na.ok`.

- null.ok:

  Passed to the backing `check_*()` function as `null.ok`.

- dim.ok:

  If `FALSE` (the default), `x` must not carry a `dim` attribute.

- class.ok:

  If `FALSE` (the default), `x` must not carry a `class` attribute.

- n.chars:

  Passed to the backing `check_*()` function as `n.chars`.

- min.chars:

  Passed to the backing `check_*()` function as `min.chars`.

- max.chars:

  Passed to the backing `check_*()` function as `max.chars`.

- pattern:

  Passed to the backing `check_*()` function as `pattern`.

- fixed:

  Passed to the backing `check_*()` function as `fixed`.

- ignore.case:

  Passed to the backing `check_*()` function as `ignore.case`.

- lower:

  Passed to the backing `check_*()` function as `lower`.

- upper:

  Passed to the backing `check_*()` function as `upper`.

- finite:

  Passed to the backing `check_*()` function as `finite`.

- tol:

  Passed to the backing `check_*()` function as `tol`.

- positive:

  Passed to the backing `check_*()` function as `positive`.

- any.missing:

  Passed to the backing `check_*()` function as `any.missing`.

- all.missing:

  Passed to the backing `check_*()` function as `all.missing`.

- len:

  Passed to the backing `check_*()` function as `len`.

- min.len:

  Passed to the backing `check_*()` function as `min.len`.

- max.len:

  Passed to the backing `check_*()` function as `max.len`.

- unique:

  Passed to the backing `check_*()` function as `unique`.

- names:

  Passed to the backing `check_*()` function as `names`.

- typed.missing:

  Passed to the backing `check_*()` function as `typed.missing`.

- sorted:

  Passed to the backing `check_*()` function as `sorted`.

- choices:

  A vector of values representing the which x must be an element of.

## Value

The original object if the assertion passes.
