# Assertion functions adapted for rlang output

Most common checkmate functions, adapted to output rlang style error
messages on failed assertions. The actual checking is done by
[`checkmate::qtest()`](https://mllg.github.io/checkmate/reference/qassert.html),
[`checkmate::check_flag()`](https://mllg.github.io/checkmate/reference/checkFlag.html)
and related functions.

|               |                      |                         |
|---------------|----------------------|-------------------------|
| **R Type**    | **Scalar**           | **Vector**              |
| `logical`     | `assert_flag(x)`     | `assert_logical(x)`     |
| `character`   | `assert_string(x)`   | `assert_character(x)`   |
| `numeric`     | `assert_number(x)`   | `assert_numeric(x)`     |
| `integer`     | `assert_inumber(x)`⁴ | `assert_integer(x)`     |
| `double`      | `assert_dnumber(x)`⁴ | `assert_double(x)`      |
| `integerish`¹ | `assert_int(x)`      | `assert_integerish(x)`  |
| `naturalish`² | `assert_count(x)`    | `assert_naturalish(x)`⁴ |
| `factor`      | ³                    | `assert_factor(x)`      |
| `complex`     | ³                    | `assert_complex(x)`     |
| `raw`         | ³                    | `assert_raw(x)`         |
| `Date`        | `assert_day(x)`      | `assert_date(x)`        |

- ¹ `integerish` refers to functional integers (numbers that are very
  close to integer values), regardless of type (`integer` or `double` )

- ² `naturalish` refers to functional integers restricted to the natural
  numbers (zero and positive numbers

- ³ No assertion functions are provided for scalar `factor`, `complex`,
  or `raw`

- ⁴ Not available in the checkmate package

|     |     |     |
|-----|-----|-----|
| a   | b   | c   |
| a   | b   | c   |

## Usage

``` r
qassert(x, ...)

assert_flag(x, ...)

assert_string(x, ...)

assert_number(x, ...)

assert_inumber(x, ...)

assert_dnumber(x, ...)

assert_int(x, ...)

assert_count(x, ...)

assert_day(x, ...)

assert_logical(x, ...)

assert_character(x, ...)

assert_numeric(x, ...)

assert_integer(x, ...)

assert_double(x, ...)

assert_integerish(x, ...)

assert_naturalish(x, ...)

assert_factor(x, ...)

assert_complex(x, ...)

assert_raw(x, ...)

assert_date(x, ...)

assert_scalar(x, ...)

assert_atomic(x, ...)

assert_list(x, ...)

assert_list(x, ...)

assert_class(x, ...)

assert_data_frame(x, ...)

assert_data_table(x, ...)

assert_tibble(x, ...)
```

## Arguments

- x:

  The variable to assert

- ...:

  Additional parameters passed to corresponding checkmate functions
  [`checkmate::qtest()`](https://mllg.github.io/checkmate/reference/qassert.html),
  [`checkmate::check_flag()`](https://mllg.github.io/checkmate/reference/checkFlag.html),
  etc.

## Value

The original object if the assertion passes.
