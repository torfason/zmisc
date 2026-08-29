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

chk_flag(x, ...)

chk_string(x, ...)

chk_number(x, ...)

chk_inumber(x, ...)

chk_dnumber(x, ...)

chk_znumber(x, ...)

chk_count(x, ...)

chk_day(x, ...)

chk_instant(x, ...)

chk_scalar(x, ...)

chk_logical(x, ...)

chk_character(x, ...)

chk_numeric(x, ...)

chk_integer(x, ...)

chk_double(x, ...)

chk_integerish(x, ...)

chk_naturalish(x, ...)

chk_factor(x, ...)

chk_complex(x, ...)

chk_raw(x, ...)

chk_date(x, ...)

chk_posixct(x, ...)

chk_atomic(x, ...)

chk_environment(x, ...)

chk_list(x, ...)

chk_data_frame(x, ...)

chk_data_table(x, ...)

chk_tibble(x, ...)

chk_class(x, ...)

chk_choice(x, choices, ...)
```

## Arguments

- x:

  The variable to assert

- ...:

  Additional parameters passed to corresponding
  [checkmate](https://mllg.github.io/checkmate/reference/checkmate-package.html)
  functions
  [`checkmate::qtest()`](https://mllg.github.io/checkmate/reference/qassert.html),
  [`checkmate::check_flag()`](https://mllg.github.io/checkmate/reference/checkFlag.html),
  etc.

- choices:

  A vector of values representing the which x must be an element of.

## Value

The original object if the assertion passes.
