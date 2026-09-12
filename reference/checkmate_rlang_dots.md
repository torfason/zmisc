# Aliases for rlang assertions

Two [rlang](https://rlang.r-lib.org/reference/rlang-package.html)
functions under the `chk_` name, for consistency with the rest of the
family.

- `chk_dots_empty()` is
  [`rlang::check_dots_empty()`](https://rlang.r-lib.org/reference/check_dots_empty.html),
  and fails if anything was passed through `...`.

- `chk_match()` is
  [`rlang::arg_match()`](https://rlang.r-lib.org/reference/arg_match.html),
  and fails unless `arg` matches one of `values`, which default to the
  values in the caller's own formals. Like every assertion here it
  returns its input, but visibly rather than invisibly, so it is written
  as `type <- chk_match(type)`. Note that `arg` must be a symbol
  (variable or function argument) representing a string, it cannot be a
  string literal.

## Usage

``` r
chk_dots_empty(
  env = caller_env(),
  error = NULL,
  call = caller_env(),
  action = abort
)

chk_match(
  arg,
  values = NULL,
  ...,
  multiple = FALSE,
  error_arg = caller_arg(arg),
  error_call = caller_env()
)
```

## Arguments

- env:

  Environment in which to look for `...`.

- error:

  An optional error handler passed to
  [`try_fetch()`](https://rlang.r-lib.org/reference/try_fetch.html). Use
  this e.g. to demote an error into a warning.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- action:

  **\[deprecated\]**

- arg:

  A symbol referring to an argument accepting strings.

- values:

  A character vector of possible values that `arg` can take.

- ...:

  These dots are for future extensions and must be empty.

- multiple:

  Whether `arg` may contain zero or several values.

- error_arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

`chk_match()` returns the matched value; `chk_dots_empty()` returns
`NULL` invisibly.

## See also

[checkmate_rlang](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
and
[checkmate_rlang_other](https://torfason.github.io/zmisc/reference/checkmate_rlang_other.md).
