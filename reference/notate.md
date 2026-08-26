# Embed factor levels and value labels in values.

This function adds level/label information as an annotation to either
factors or `labelled` variables. This function is called `notate()`
rather than `annotate()` to avoid conflict with `ggplot2::annotate()`.
It is a generic that can operate either on individual vectors or on a
`data.frame`.

When printing `labelled` variables from a `tibble` in a console, both
the numeric value and the text label are shown, but no variable labels.
When using the [`View()`](https://rdrr.io/r/utils/View.html) function,
only variable labels are shown but no value labels. For factors, there
is no way to view the integer levels and values at the same time.

In order to allow the viewing of both variable and value labels at the
same time, this function converts both `factor` and `labelled` variables
to `character`, including both numeric levels (`labelled` values) and
character values (`labelled` labels) in the output.

## Usage

``` r
notate(x)
```

## Arguments

- x:

  The object (either vector or `date.frame` of vectors), that one
  desires to annotate and/or view.

## Value

The processed `data.frame`, suitable for viewing, in particular through
the [`View()`](https://rdrr.io/r/utils/View.html) function.

## Examples

``` r
if (getRversion() >= "4") {
  d <- data.frame(
    chr = letters[1:4],
    fct = factor(c("alpha", "bravo", "chrly", "delta")),
    lbl = ll_labelled(c(1, 2, 3, NA),
                      labels = c(one=1, two=2),
                      label = "A labelled vector")
  )
  dn <- notate(d)
  dn
  # View(dn)
}
#>   chr       fct     lbl
#> 1   a [1] alpha [1] one
#> 2   b [2] bravo [2] two
#> 3   c [3] chrly     [3]
#> 4   d [4] delta    <NA>
```
