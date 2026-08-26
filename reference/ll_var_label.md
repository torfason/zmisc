# Get or set variable label of a labelled variable

Gets or sets the variable label (`label` attribute) of a labelled
vector. The getters/setters should be used rather than manipulating
attributes directly, since these functions perform checks to ensure that
the result, and the resulting labelled variable, are valid.

## Usage

``` r
ll_var_label(x)

ll_var_label(x) <- value
```

## Arguments

- x:

  A labelled variable

## See also

Other labelled light:
[`ll_assert_labelled()`](https://torfason.github.io/zmisc/reference/ll_assert_labelled.md),
[`ll_labelled()`](https://torfason.github.io/zmisc/reference/ll_labelled.md),
[`ll_to_character()`](https://torfason.github.io/zmisc/reference/ll_to_character.md),
[`ll_val_labels()`](https://torfason.github.io/zmisc/reference/ll_val_labels.md),
[`threadbare()`](https://torfason.github.io/zmisc/reference/threadbare.md)
