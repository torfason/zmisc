# Create a labelled variable

The labelled_light (ll) collection is a minimal implementation of core
functions for creating and managing `haven_labelled` variables, and with
minimal dependencies. These functions, prefixed with `ll_` rely only on
base R, and operate only on objects of type `haven_labelled`. All
functions check internally that the variables have the correct class and
the correct structure for labelled variables, satisfying the (minimal)
specification inherent in the parameter documentation of the
[`haven::labelled()`](https://haven.tidyverse.org/reference/labelled.html)
function.

The constructor, `ll_labelled()`, creates a labelled variable satisfying
that specification.

## Usage

``` r
ll_labelled(x = double(), labels = NULL, label = NULL)
```

## Arguments

- x:

  A vector to label. Must be either numeric (integer or double) or
  character.

- labels:

  A named vector or `NULL`. The vector should be the same type as `x`.
  Unlike factors, labels don't need to be exhaustive: only a fraction of
  the values might be labelled.

- label:

  A short, human-readable description of the vector.

## Value

A valid labelled variable.

## See also

Other labelled light:
[`ll_chk_labelled()`](https://torfason.github.io/zmisc/reference/ll_chk_labelled.md),
[`ll_to_character()`](https://torfason.github.io/zmisc/reference/ll_to_character.md),
[`ll_val_labels()`](https://torfason.github.io/zmisc/reference/ll_val_labels.md),
[`ll_var_label()`](https://torfason.github.io/zmisc/reference/ll_var_label.md),
[`threadbare()`](https://torfason.github.io/zmisc/reference/threadbare.md)
