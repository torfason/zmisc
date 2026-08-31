# Get or set value labels of a labelled variable

Gets or sets the value labels (`labels` attribute) of a labelled vector.
The getters/setters should be used rather than manipulating attributes
directly, since these functions perform checks to ensure that the
result, and the resulting labelled variable, are valid.

## Usage

``` r
ll_val_labels(x, always = FALSE)

ll_val_labels(x) <- value
```

## Arguments

- x:

  A labelled variable

- always:

  Always return at least an empty vector of the correct type, even if
  the attribute is not set.

## See also

Other labelled light:
[`ll_chk_labelled()`](https://torfason.github.io/zmisc/reference/ll_chk_labelled.md),
[`ll_labelled()`](https://torfason.github.io/zmisc/reference/ll_labelled.md),
[`ll_to_character()`](https://torfason.github.io/zmisc/reference/ll_to_character.md),
[`ll_var_label()`](https://torfason.github.io/zmisc/reference/ll_var_label.md),
[`threadbare()`](https://torfason.github.io/zmisc/reference/threadbare.md)
