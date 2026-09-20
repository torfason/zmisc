# Changelog

## zmisc 0.2.4

### New features

- Adds argument check functions, `chk_*()`. The family covers the atomic
  types in both scalar and vector form
  ([`chk_string()`](https://torfason.github.io/zmisc/reference/chk_atomic.md),
  [`chk_character()`](https://torfason.github.io/zmisc/reference/chk_atomic.md),
  [`chk_number()`](https://torfason.github.io/zmisc/reference/chk_atomic.md),
  and so on), lists and composite objects
  ([`chk_list()`](https://torfason.github.io/zmisc/reference/chk_composite.md),
  [`chk_data_frame()`](https://torfason.github.io/zmisc/reference/chk_composite.md),
  [`chk_tibble()`](https://torfason.github.io/zmisc/reference/chk_composite.md),
  …), classes and arbitrary conditions
  ([`chk_class()`](https://torfason.github.io/zmisc/reference/chk_other.md),
  [`chk_true()`](https://torfason.github.io/zmisc/reference/chk_other.md),
  [`chk_that()`](https://torfason.github.io/zmisc/reference/chk_other.md)),
  and
  [`chk_any()`](https://torfason.github.io/zmisc/reference/chk_other.md)
  for a requirement that any one of several checks may satisfy. See
  [`vignette("chk")`](https://torfason.github.io/zmisc/articles/chk.md),
  and
  [`?chk_atomic`](https://torfason.github.io/zmisc/reference/chk_atomic.md),
  [`?chk_composite`](https://torfason.github.io/zmisc/reference/chk_composite.md)
  and
  [`?chk_other`](https://torfason.github.io/zmisc/reference/chk_other.md).

- Adds
  [`glue_vector()`](https://torfason.github.io/zmisc/reference/glue_vector.md),
  which applies a `glue` template element-wise to a vector.
  [`glue()`](https://glue.tidyverse.org/reference/glue.html) and
  [`glue_data()`](https://glue.tidyverse.org/reference/glue.html) are
  re-exported alongside it.

- Adds
  [`asciify()`](https://torfason.github.io/zmisc/reference/asciify.md),
  which transliterates accented characters to ASCII.

- Adds
  [`yencode()`](https://torfason.github.io/zmisc/reference/yencode.md)
  and
  [`ydecode()`](https://torfason.github.io/zmisc/reference/yencode.md),
  which encode strings into a restricted character set and back.

### Bug fixes and behavior changes

- Various fixes and improvements to zeq(), zample() and zingle().

- `ll_assert_labelled()` is renamed to
  [`ll_chk_labelled()`](https://torfason.github.io/zmisc/reference/ll_chk_labelled.md),
  following the `chk_` naming used for the new check functions.

- [`wrap_error()`](https://torfason.github.io/zmisc/reference/wrap_error.md)
  now honors its `wrap` argument correctly.

## zmisc 0.2.3

CRAN release: 2023-08-22

- Adds the notate() function.

- Adds a default parameter to the lookup() and lookuper() functions.

## zmisc 0.2.2

CRAN release: 2022-04-29

- This version fixes an incompatibility with roxygen 7.1.3.

## zmisc 0.2.1

CRAN release: 2022-02-02

- This version includes documentation improvements as well as a few
  other fixes.

## zmisc 0.2.0

- This version adds two functions,
  [`lookup()`](https://torfason.github.io/zmisc/reference/lookup.md) and
  [`lookuper()`](https://torfason.github.io/zmisc/reference/lookup.md),
  designed to look up values from a lookup table in a way that works
  smoothly in an R workflow, in particular within pipes.

## zmisc 0.1.0

- This is the initial version of zmisc, which includes three functions,
  [`zample()`](https://torfason.github.io/zmisc/reference/zample.md),
  [`zeq()`](https://torfason.github.io/zmisc/reference/zeq.md), and
  [`zingle()`](https://torfason.github.io/zmisc/reference/zingle.md),
  for safer sampling, sequencing and subsetting in a data processing
  workflow
