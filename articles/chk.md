# Checks with rlang-style errors

``` r

library(zmisc)
```

The `chk_*()` functions check the type and shape of an argument and, on
failure, raise an [rlang](https://rlang.r-lib.org/)-style error naming
the argument as the caller wrote it. The checking itself is done by
[checkmate](https://mllg.github.io/checkmate/). Each function returns
its input, so a check can sit in the middle of a pipe, and each is cheap
enough on the passing path to leave at the top of any function: the
passing case is one call to the backing `check_*()`, a test, and a
return. Assembling a good message happens on the failing path, which
runs once and then stops.

A passing check returns its input invisibly, so on its own it prints
nothing.

``` r

# The check returns its input, so it composes in a pipe
c(2, 4, 6) |> chk_numeric(length = 3) |> sum()
#> [1] 12

# On failure, the error names the argument as the caller wrote it
my_mean <- function(x) {
  chk_numeric(x)
  sum(x) / length(x)
}
tryCatch(my_mean("seven"), error = wrap_error)
#> #E> Assertion on `x` failed: • Must be of
#> #E> type 'numeric', not 'character'
```

## Scalars and atomic vectors

Every atomic type is covered in both a scalar and a vector form.

| R type | Scalar | Vector |
|----|----|----|
| any type | [`chk_scalar()`](https://torfason.github.io/zmisc/reference/chk_atomic.md) | [`chk_atomic()`](https://torfason.github.io/zmisc/reference/chk_atomic.md) |
| `logical` | [`chk_flag()`](https://torfason.github.io/zmisc/reference/chk_atomic.md) | [`chk_logical()`](https://torfason.github.io/zmisc/reference/chk_atomic.md) |
| `character` | [`chk_string()`](https://torfason.github.io/zmisc/reference/chk_atomic.md) | [`chk_character()`](https://torfason.github.io/zmisc/reference/chk_atomic.md) |
| `numeric` | [`chk_number()`](https://torfason.github.io/zmisc/reference/chk_atomic.md) | [`chk_numeric()`](https://torfason.github.io/zmisc/reference/chk_atomic.md) |
| `integer` | [`chk_inumber()`](https://torfason.github.io/zmisc/reference/chk_atomic.md) | [`chk_integer()`](https://torfason.github.io/zmisc/reference/chk_atomic.md) |
| `double` | [`chk_dnumber()`](https://torfason.github.io/zmisc/reference/chk_atomic.md) | [`chk_double()`](https://torfason.github.io/zmisc/reference/chk_atomic.md) |
| integerish | [`chk_znumber()`](https://torfason.github.io/zmisc/reference/chk_atomic.md) | [`chk_integerish()`](https://torfason.github.io/zmisc/reference/chk_atomic.md) |
| naturalish | [`chk_count()`](https://torfason.github.io/zmisc/reference/chk_atomic.md) | [`chk_naturalish()`](https://torfason.github.io/zmisc/reference/chk_atomic.md) |
| `factor` |  | [`chk_factor()`](https://torfason.github.io/zmisc/reference/chk_atomic.md) |
| `complex` |  | [`chk_complex()`](https://torfason.github.io/zmisc/reference/chk_atomic.md) |
| `raw` |  | [`chk_raw()`](https://torfason.github.io/zmisc/reference/chk_atomic.md) |
| `Date` | [`chk_day()`](https://torfason.github.io/zmisc/reference/chk_atomic.md) | [`chk_date()`](https://torfason.github.io/zmisc/reference/chk_atomic.md) |
| `POSIXct` | [`chk_instant()`](https://torfason.github.io/zmisc/reference/chk_atomic.md) | [`chk_posixct()`](https://torfason.github.io/zmisc/reference/chk_atomic.md) |

*integerish* means a functional integer: a number very close to a whole
number, whether stored as `integer` or as `double`. *naturalish*
restricts that to the natural numbers, zero and up.

Every one of them takes the same set of arguments: `na.ok`, `null.ok`,
`attr.ok`, and `length` or `range` where they apply, plus `zero.ok` for
the naturalish pair. The dots are reserved, so a misspelled argument
raises rather than being quietly ignored.

``` r

tryCatch(chk_character(c("a", NA), na.ok = FALSE), error = wrap_error)
#> #E> Assertion on `c("a", NA)` failed: •
#> #E> Contains missing values (element 2)
tryCatch(chk_integer(1:5, length = 3), error = wrap_error)
#> #E> Assertion on `1:5` failed: • Must have
#> #E> length 3, but has length 5
tryCatch(chk_numeric(c(1, 99), range = c(0, 10)), error = wrap_error)
#> #E> Assertion on `c(1, 99)` failed: • Element
#> #E> 2 is not <= 10
tryCatch(chk_count(0, zero.ok = FALSE), error = wrap_error)
#> #E> Assertion on `0` failed: • Must be >= 1
```

`length` and `range` are pairs. A scalar pins both ends, `NA` at an end
means no bound there, and the same rule applies whether the pair counts
elements, counts characters, or bounds values.

``` r

chk_character(letters, length = c(10, NA)) |> length()
#> [1] 26
chk_string("abc", range = c(1, 3)) |> nchar()
#> [1] 3
```

`attr.ok` lists the attributes `x` may carry beyond those intrinsic to
its type, defaults to `"names"`, and takes `FALSE` for none at all or
`TRUE` for any.

``` r

labelled_ages <- structure(c(38L, 41L), label = "Age at interview")
tryCatch(chk_integer(labelled_ages), error = wrap_error)
#> #E> Assertion on `labelled_ages` failed: •
#> #E> Must not have attributes: label
chk_integer(labelled_ages, attr.ok = "label") |> sum()
#> [1] 79
```

## Lists and composite objects

Container checks take `null.ok`.
[`chk_list()`](https://torfason.github.io/zmisc/reference/chk_composite.md)
additionally takes `length` with the same semantics as for atomic
vectors.
[`chk_environment()`](https://torfason.github.io/zmisc/reference/chk_composite.md)
additionally takes `contains` as a list of item names that must be
present in the environment.

``` r

chk_data_frame(mtcars) |> nrow()
#> [1] 32
chk_list(list(a = 1, b = 2), length = 2) |> names()
#> [1] "a" "b"

# A data.frame is a list to typeof(), but not to chk_list()
tryCatch(chk_list(mtcars), error = wrap_error)
#> #E> Assertion on `mtcars` failed: • Must be
#> #E> of type 'list', not 'data.frame' • Must
#> #E> not have a class attribute, but has class
#> #E> "data.frame" • Must not have attributes:
#> #E> row.names
```

[`chk_environment()`](https://torfason.github.io/zmisc/reference/chk_composite.md),
[`chk_data_table()`](https://torfason.github.io/zmisc/reference/chk_composite.md)
and
[`chk_tibble()`](https://torfason.github.io/zmisc/reference/chk_composite.md)
complete the set.

## Classes and conditions

[`chk_class()`](https://torfason.github.io/zmisc/reference/chk_other.md)
checks inheritance, and
[`chk_true()`](https://torfason.github.io/zmisc/reference/chk_other.md)
is the catch-all: any property of any object that can be written as a
condition, at the cost of a message that can only report that the
condition was not met.

``` r

tryCatch(chk_class(1:3, "factor"), error = wrap_error)
#> #E> Assertion on `1:3` failed: • Must inherit
#> #E> from class 'factor', but has class 'integer'
tryCatch(chk_true(nrow(mtcars) > 100), error = wrap_error)
#> #E> Assertion on `nrow(mtcars) > 100` failed:
#> #E> • Must be TRUE
```

[`chk_that()`](https://torfason.github.io/zmisc/reference/chk_other.md)
is parallel to
[`chk_true()`](https://torfason.github.io/zmisc/reference/chk_other.md),
but with the value and the expression separated, so that the condition
can be applied to an object passing through a pipe. The value is bound
to `.` by default.

``` r

mtcars |> chk_that(nrow(.) > 10) |> ncol()
#> [1] 11
tryCatch(mtcars |> chk_that(nrow(.) > 100), error = wrap_error)
#> #E> Assertion on `nrow(.) > 100` failed: •
#> #E> Must be TRUE
```

[`chk_dots_empty()`](https://torfason.github.io/zmisc/reference/chk_other.md)
fails if anything was passed through `...`, and
[`chk_match()`](https://torfason.github.io/zmisc/reference/chk_other.md)
matches an argument against the values in its own default, returning the
match.
[`chk_match()`](https://torfason.github.io/zmisc/reference/chk_other.md)
can be used *either* instead of
[`match.arg()`](https://rdrr.io/r/base/match.arg.html) for a choice
style argument (in which case `x` must be a symbol), or as an way to
check that an arbitrary `character` value is an element of a set.

``` r

plot_kind <- function(kind = c("scatter", "line", "bar")) {
  kind <- chk_match(kind)
  kind
}
plot_kind()
#> [1] "scatter"
tryCatch(plot_kind("pie"), error = wrap_error)
#> #E> Assertion on `kind` failed: • Must be one
#> #E> of "scatter", "line", or "bar", not "pie"
```

## Alternatives

Each `chk_*()` function states one thing. A requirement that is
satisfied by either of two shapes is written with
[`chk_any()`](https://torfason.github.io/zmisc/reference/chk_other.md),
which evaluates its arguments in turn, returns the value of the first
that passes, and otherwise raises one error reporting every failure.

``` r

x <- "a"
chk_any(chk_string(x), chk_number(x)) |> toupper()
#> [1] "A"

y <- TRUE
tryCatch(chk_any(chk_string(y), chk_number(y)), error = wrap_error)
#> #E> Assertion on `y` failed, none of the alternatives
#> #E> passed: • Must be of type 'string', not
#> #E> 'logical' • Must be of type 'number', not
#> #E> 'logical'
```

[`chk_any()`](https://torfason.github.io/zmisc/reference/chk_other.md)
relies on other `chk_*()` functions being aware that they are being
called by
[`chk_any()`](https://torfason.github.io/zmisc/reference/chk_other.md)
and does not catch arbitrary errors. An error is never raised unless all
the checks fail, so a composite
[`chk_any()`](https://torfason.github.io/zmisc/reference/chk_other.md)
call is not unbearably slow.
[`chk_any()`](https://torfason.github.io/zmisc/reference/chk_other.md)
on two passing elementary checks takes 10-20 microseconds compared to
1-4 microseconds for the elementary checks themselves. If the first
elementary check fails, this goes up to around 50 microseconds, compared
with around a millisecond (1000 microseconds) for a minimally caught
actual error.

Only the direct
[`chk_any()`](https://torfason.github.io/zmisc/reference/chk_other.md)
calls itself are handled. Calls reached through a helper function, or
from inside a lambda passed to
[`lapply()`](https://rdrr.io/r/base/lapply.html) will error directly,
and so does everything that is not a failed check: a misspelled
function, an argument that does not exist, an object that was never
bound.

The arguments are captured as expressions and evaluated in the calling
environment, so `...` cannot be forwarded into
[`chk_any()`](https://torfason.github.io/zmisc/reference/chk_other.md)
from another function, and an object cannot be piped into it. Both raise
an error.

## Reference

The full argument documentation is in the help files, one per group:
[chk_atomic](https://torfason.github.io/zmisc/reference/chk_atomic.html),
[chk_composite](https://torfason.github.io/zmisc/reference/chk_composite.html)
and
[chk_other](https://torfason.github.io/zmisc/reference/chk_other.html).
