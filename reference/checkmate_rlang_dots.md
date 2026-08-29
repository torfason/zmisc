# Assert that no dots arguments are passed

`chk_dots_empty()` is an alias for
[`rlang::check_dots_empty()`](https://rlang.r-lib.org/reference/check_dots_empty.html),
provided for naming consistency with other assertion functions. It
throws an error if any arguments are passed through `...`.

## Usage

``` r
chk_dots_empty(
  env = caller_env(),
  error = NULL,
  call = caller_env(),
  action = abort
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
