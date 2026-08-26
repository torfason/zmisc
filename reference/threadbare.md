# Return a threadbare version of a vector

A bare object is an R object that has no class attributes (see
[`rlang::is_bare_character()`](https://rlang.r-lib.org/reference/bare-type-predicates.html)).
A threadbare object is an atomic object (i.e. not a
[`list()`](https://rdrr.io/r/base/list.html), see
[`is.atomic()`](https://rdrr.io/r/base/is.recursive.html)), with no
attributes at all. The function returns an error if a list is passed.

## Usage

``` r
threadbare(x)
```

## Arguments

- x:

  A vector, possibly classed, but not a list object, to strip of all
  attributes.

## Value

A vector with the same core values as `x`, but with no
[`attributes()`](https://rdrr.io/r/base/attributes.html) at all, not
even [`names()`](https://rdrr.io/r/base/names.html).

## See also

Other labelled light:
[`ll_assert_labelled()`](https://torfason.github.io/zmisc/reference/ll_assert_labelled.md),
[`ll_labelled()`](https://torfason.github.io/zmisc/reference/ll_labelled.md),
[`ll_to_character()`](https://torfason.github.io/zmisc/reference/ll_to_character.md),
[`ll_val_labels()`](https://torfason.github.io/zmisc/reference/ll_val_labels.md),
[`ll_var_label()`](https://torfason.github.io/zmisc/reference/ll_var_label.md)
