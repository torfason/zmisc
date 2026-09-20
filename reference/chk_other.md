# Various other check functions

Various checks for classes and other, together with the two
[rlang](https://rlang.r-lib.org/reference/rlang-package.html) assertions
carried under the `chk_` name. See
[chk_atomic](https://torfason.github.io/zmisc/reference/chk_atomic.md)
for the scalar and vector types.

|                         |                                            |
|-------------------------|--------------------------------------------|
| **Function**            | **Passes when**                            |
| `chk_class(x, classes)` | `x` inherits from every class in `classes` |
| `chk_true(x)`           | `x` is `TRUE`                              |
| `chk_that(x, expr)`     | `expr`, with `.` bound to `x`, is `TRUE`   |
| `chk_dots_empty()`      | nothing was passed through `...`           |
| `chk_match(arg)`        | `arg` matches one of `values`              |
| `chk_any(...)`          | at least one of the checks given passes    |

`chk_true()` is the catch-all: any property of any object that can be
written as a condition, at the cost of a message that can only report
that the condition was not met. `chk_that()` also checks that an
expression is true, but separates the value to be checked (`x`) from the
expression evaluated on it (`expr`), which makes an arbitrary condition
usable on an object passing through a pipe.

`chk_dots_empty()` is equivalent to
[`rlang::check_dots_empty()`](https://rlang.r-lib.org/reference/check_dots_empty.html)
and `chk_match()` is equivalent to
[`rlang::arg_match()`](https://rlang.r-lib.org/reference/arg_match.html).
Like every check here `chk_match()` returns its input, but visibly
rather than invisibly, so it is written as `type <- chk_match(type)`.
Its `arg` must be a symbol standing for a string, not a string literal.

Two assertions for the use cases of
[`rlang::arg_match()`](https://rlang.r-lib.org/reference/arg_match.html)
and
[`rlang::check_dots_empty()`](https://rlang.r-lib.org/reference/check_dots_empty.html).

## Usage

``` r
chk_any(...)

chk_class(x, classes, ..., null.ok = FALSE, ordered = FALSE)

chk_true(x, ..., na.ok = FALSE)

chk_that(x, expr, ..., na.ok = FALSE, .varnames = ".")

chk_match(x, values = NULL, ..., multiple = FALSE, error_arg = NULL)

chk_dots_empty()
```

## Arguments

- ...:

  These dots are for future extensions and must be empty.

- x:

  Value to check and match. A string, or the untouched default of the
  argument being matched.

- classes:

  Character vector of class names `x` must inherit from.

- null.ok:

  Is `NULL` permitted?

- ordered:

  Must `classes` appear in that order at the head of `class(x)`?

- na.ok:

  Are missing values permitted?

- expr:

  Expression to evaluate on `x`, which is bound to `.` unless
  `.varnames` says otherwise.

- .varnames:

  Names to bind `x` to when evaluating `expr`.

- values:

  Permitted values, as a character vector. `NULL` reads them from the
  default of the caller's formal named by `x`, which then has to be a
  symbol.

- multiple:

  Is `x` allowed to hold several values? Each must then match, and all
  are returned.

- error_arg:

  Name to report the failure against, in place of the expression `x` was
  written as.

## Value

The original object if the check passes. `chk_match()` returns the
matched value visibly, `chk_dots_empty()` returns `NULL` invisibly, and
`chk_any()` returns the value of the first argument that passes, which
is the object that was checked.

`chk_match()` returns the matched value; `chk_dots_empty()` returns
`NULL` invisibly.

## Details

`chk_any()` evaluates its arguments in turn and returns the value of the
first that passes. If none pass, it raises one error reporting every
failure. It is how a composite requirement is written, where each
individual `chk_*()` function states only one thing:

    chk_any(chk_string(x), chk_number(x))

Only the checks `chk_any()` calls itself are candidates. One reached
through a helper function, or from inside a lambda passed to
[`lapply()`](https://rdrr.io/r/base/lapply.html), throws where it
stands, and so does everything that is not a failed check: a misspelled
function, an argument that does not exist, an object that was never
bound.

The arguments are captured as expressions and evaluated in the calling
environment, which rules out two ways of reaching `chk_any()`
indirectly. `...` cannot be forwarded into it from another function, and
an object cannot be piped into it. Both raise an error rather than being
accommodated, since the first would evaluate the checks in the wrong
scope and the second would return the piped object as a branch that
passed. Write the checks at the call site, naming the object in each.

- `chk_match()` looks up `x` from `values`, which default to the values
  in the caller's own formals, and fails if it is not matched. To assign
  a default value to arg, call with `arg <- chk_match(arg)`. If `values`
  is not given, `x` must be a symbol, since the values are then read
  from the formal of that name.

- `chk_dots_empty()` fails if anything was passed through `...`.

## See also

[chk_atomic](https://torfason.github.io/zmisc/reference/chk_atomic.md)
for the scalar and vector types, and
[chk_composite](https://torfason.github.io/zmisc/reference/chk_composite.md)
for lists and composite objects.

[chk_atomic](https://torfason.github.io/zmisc/reference/chk_atomic.md)
and
[chk_composite](https://torfason.github.io/zmisc/reference/chk_composite.md).

## Examples

``` r
# Any property that can be written as a condition
chk_true(nrow(mtcars) > 10)
mtcars |> chk_that(nrow(.) > 10) |> ncol()
#> [1] 11

# Match an argument against the values in its own default
plot_kind <- function(kind = c("scatter", "line", "bar")) chk_match(kind)
plot_kind()
#> [1] "scatter"
tryCatch(plot_kind("pie"), error = wrap_error)
#> #E> Assertion on `kind` failed: • Must be one
#> #E> of "scatter", "line", or "bar", not "pie"

# chk_any() takes the first check that passes
x <- "a"
chk_any(chk_string(x), chk_number(x))
tryCatch(chk_any(chk_number(x), chk_logical(x)), error = wrap_error)
#> #E> Assertion on `x` failed, none of the alternatives
#> #E> passed: • Must be of type 'number', not
#> #E> 'character' • Must be of type 'logical',
#> #E> not 'character'
```
