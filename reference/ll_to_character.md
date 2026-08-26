# Get the character representation of a labelled variable

Returns a character representation of a labelled variable, using the
value labels to look up the label for a given value.

The default behavior of this function is similar to
[`labelled::to_character()`](https://larmarange.github.io/labelled/reference/to_character.html).
The options, however, are slightly different. Most importantly, instead
of specifying `NA` handling using parameters, the function relies on the
`default` parameter to determine what happens for unlabelled variables,
allowing users to specify including the original values of `x` instead
of the labels, returning `NA`, or returning a specific string value.
Also, the default behavior is to drop any variable label attribute, in
line with the default
[`as.character()`](https://rdrr.io/r/base/character.html) method.

## Usage

``` r
ll_to_character(x, default = x, preserve_var_label = FALSE)
```

## Arguments

- x:

  A labelled variable

- default:

  Vector providing a default label for any values not found in the
  `val_labels` (unlabelled values). Must be of length 1 or of the same
  length as x. Useful possibilities are `x` (use values where labels are
  not found), `NA` (return NA for such values), and `""` (an empty
  string). Missing (`NA`) values in `x`, however, are never replaced
  with the default, they remain `NA`.

- preserve_var_label:

  Should any `var_label` in x be preserved, or should they be dropped
  from the result (ensuring that the result is bare and without any
  attributes).

## See also

Other labelled light:
[`ll_assert_labelled()`](https://torfason.github.io/zmisc/reference/ll_assert_labelled.md),
[`ll_labelled()`](https://torfason.github.io/zmisc/reference/ll_labelled.md),
[`ll_val_labels()`](https://torfason.github.io/zmisc/reference/ll_val_labels.md),
[`ll_var_label()`](https://torfason.github.io/zmisc/reference/ll_var_label.md),
[`threadbare()`](https://torfason.github.io/zmisc/reference/threadbare.md)
