# Assert that at least one of several assertions passes

`chk_any()` evaluates its arguments in turn and returns the value of the
first assertion that passes. If none pass, it raises one error reporting
every failure. It is how a composite requirement is written, where each
`chk_*()` function states only one thing:

    chk_any(chk_string(x), chk_number(x))

## Usage

``` r
chk_any(...)
```

## Arguments

- ...:

  Assertion calls. Evaluated left to right, stopping at the first that
  passes. Must not be named.

## Value

The value of the first argument that passes, invisibly. This is the
object that was asserted on, so `chk_any()` can be used inline the same
way the individual assertions can.

## Details

Only the assertions `chk_any()` calls itself are candidates. An
assertion reached through a helper function, or from inside a lambda
passed to [`lapply()`](https://rdrr.io/r/base/lapply.html), throws where
it stands, and so does everything that is not an assertion failure: a
misspelled function, an argument that does not exist, an object that was
never bound. That is the difference between this and wrapping the
branches in [`tryCatch()`](https://rdrr.io/r/base/conditions.html),
which cannot tell a failed check from a typo, and it is also why the
passing case costs microseconds rather than the milliseconds an
[`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html) spends
capturing a backtrace.

The arguments are captured as expressions and evaluated in the calling
environment, which rules out two ways of reaching `chk_any()`
indirectly. `...` cannot be forwarded into it from another function, and
an object cannot be piped into it. Both raise an error rather than being
accommodated, since the first would evaluate the assertions in the wrong
scope and the second would return the piped object as a branch that
passed. Write the assertions at the call site, naming the object in
each.

## See also

[checkmate_rlang](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
for the scalar and vector types,
[checkmate_rlang_other](https://torfason.github.io/zmisc/reference/checkmate_rlang_other.md)
for containers and
[`chk_true()`](https://torfason.github.io/zmisc/reference/checkmate_rlang_other.md).

## Examples

``` r
x <- "a"
chk_any(chk_string(x), chk_number(x))

y <- 3
chk_any(chk_string(y), chk_number(y))
```
