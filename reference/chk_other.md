# Various other check functions

Various checks not directly related to atomic vectors (see
[chk_atomic](https://torfason.github.io/zmisc/reference/chk_atomic.md)),
or composite objects (see
[chk_composite](https://torfason.github.io/zmisc/reference/chk_composite.md)).

|                         |                                            |
|-------------------------|--------------------------------------------|
| **Function**            | **Passes when**                            |
| `chk_true(x)`           | `x` is `TRUE` (implement arbitrary checks) |
| `chk_that(x, expr)`     | `expr`, with `.` bound to `x`, is `TRUE`   |
| `chk_class(x, classes)` | `x` inherits from every class in `classes` |
| `chk_match(x, values)`  | `x` matches one of `values`                |
| `chk_dots_empty()`      | nothing was passed through `...`           |
| `chk_any(...)`          | at least one of the checks given passes    |

`chk_true()` is a catch-all function that can be used to implement
arbitrary checks (by checking any expression that should be true).

`chk_that()` provides an alternative for checking that an expression is
true, but separates the value to be checked (`x`) from the expression
evaluated on it (`expr`), which makes an arbitrary condition usable on
an object passing through a pipe.

`chk_class()` provides a quick way to check the class of an object.

`chk_match()` can be used either as an equivalent to
[`rlang::arg_match()`](https://rlang.r-lib.org/reference/arg_match.html)
or [`match.arg()`](https://rdrr.io/r/base/match.arg.html) for checking a
function argument against default values in a function, or to check that
any (`character`) variable x is an element of a (`character`) vector of
`values`. When used to select default value in a function, it must be
called as `arg <- chk_match(arg)`.

`chk_dots_empty()` verifies that no arguments were passed to the `...`
parameters, similarly to
[`rlang::check_dots_empty()`](https://rlang.r-lib.org/reference/check_dots_empty.html).

`chk_any(...)` can be used to combine multiple other checks, and will
fail only if all constituent checks fail.

## Usage

``` r
chk_true(x, ..., na.ok = FALSE)

chk_that(x, expr, ..., na.ok = FALSE, bindings = ".")

chk_class(x, classes, ..., null.ok = FALSE, ordered = FALSE)

chk_match(x, values = NULL, ..., multiple = FALSE)

chk_dots_empty()

chk_any(...)
```

## Arguments

- x:

  Object to check.

- ...:

  For `chk_any()`, the checks to try (see examples). For every other
  function here the dots must be empty.

- na.ok:

  Are missing values permitted?

- expr:

  Expression to evaluate on `x`, which is bound to `.` unless `bindings`
  says otherwise.

- bindings:

  Character vector with name (or names) to bind `x` to when evaluating
  `expr`.

- classes:

  Character vector of class names `x` must inherit from.

- null.ok:

  Is `NULL` permitted?

- ordered:

  Must `classes` appear in that order at the head of `class(x)`?

- values:

  Character vector of values which the object checked by `chk_match()`
  must be an element of.

- multiple:

  Logical determining if the return value of `chk_match()` can contain
  more than one element.

## Value

The original object if the check passes. `chk_match()` returns the
matched value visibly, `chk_dots_empty()` returns `NULL` invisibly, and
`chk_any()` returns the value of the first argument that passes, which
is the object that was checked.

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

## See also

[chk_atomic](https://torfason.github.io/zmisc/reference/chk_atomic.md)
for the scalar and vector types, and
[chk_composite](https://torfason.github.io/zmisc/reference/chk_composite.md)
for lists and composite objects.

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
