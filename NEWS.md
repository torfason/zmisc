
# zmisc 0.2.4

## New features

* Adds argument check functions, `chk_*()`. The family covers the atomic types in
  both scalar and vector form (`chk_string()`, `chk_character()`,
  `chk_number()`, and so on), lists and composite objects (`chk_list()`,
  `chk_data_frame()`, `chk_tibble()`, ...), classes and arbitrary conditions
  (`chk_class()`, `chk_true()`, `chk_that()`), and `chk_any()` for a requirement
  that any one of several checks may satisfy. See `vignette("chk")`, and
  `?chk_atomic`, `?chk_composite` and `?chk_other`.

* Adds `glue_vector()`, which applies a `glue` template element-wise to
  a vector. `glue()` and `glue_data()` are re-exported alongside it.

* Adds `asciify()`, which transliterates accented characters to ASCII.

* Adds `yencode()` and `ydecode()`, which encode strings into a
  restricted character set and back.

## Bug fixes and behavior changes

* Various fixes and improvements to zeq(), zample() and zingle().

* `ll_assert_labelled()` is renamed to `ll_chk_labelled()`, following
  the `chk_` naming used for the new check functions.

* `wrap_error()` now honors its `wrap` argument correctly.

# zmisc 0.2.3

* Adds the notate() function.

* Adds a default parameter to the lookup() and lookuper() 
  functions.


# zmisc 0.2.2

* This version fixes an incompatibility with roxygen 7.1.3.


# zmisc 0.2.1

* This version includes documentation improvements as well as
  a few other fixes.


# zmisc 0.2.0

* This version adds two functions, `lookup()` and `lookuper()`,
  designed to look up values from a lookup table in a way that 
  works smoothly in an R workflow, in particular within pipes.


# zmisc 0.1.0

* This is the initial version of zmisc, which includes three
  functions, `zample()`, `zeq()`, and `zingle()`, for safer
  sampling, sequencing and subsetting in a data processing
  workflow
  

