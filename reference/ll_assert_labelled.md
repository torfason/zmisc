# Verify that x is a valid labelled variable

Verify that x is a valid labelled variable satisfying the (minimal)
specification inherent in the parameter documentation of the
[`haven::labelled()`](https://haven.tidyverse.org/reference/labelled.html)
function for `haven_labelled` objects.

## Usage

``` r
ll_assert_labelled(x)
```

## Arguments

- x:

  A labelled variable

## Value

Invisibly returns x if the check is successful.

## See also

Other labelled light:
[`ll_labelled()`](https://torfason.github.io/zmisc/reference/ll_labelled.md),
[`ll_to_character()`](https://torfason.github.io/zmisc/reference/ll_to_character.md),
[`ll_val_labels()`](https://torfason.github.io/zmisc/reference/ll_val_labels.md),
[`ll_var_label()`](https://torfason.github.io/zmisc/reference/ll_var_label.md),
[`threadbare()`](https://torfason.github.io/zmisc/reference/threadbare.md)
