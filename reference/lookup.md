# Lookup values from a lookup table

The `lookup()` function implements lookup of values (such as variable
names) from a lookup table which maps keys onto values (such as variable
labels or descriptions).

The lookup table can be in the form of a two-column `data.frame`, in the
form of a named `vector`, or in the form of a `list`. If the table is in
the form of a `data.frame`, the key column should be named either `key`
or `name`, and the value column should be named `value` (for the value).
If the lookup table is in the form of a named `vector` or `list`, the
names are used as the key, and the returned value is taken from the
values in the vector or list.

The underlying lookup is done using
[`base::match()`](https://rdrr.io/r/base/match.html), and all atomic
data types except `factor` are supported. Factors are omitted due to the
ambiguity in what should be looked up (the values or the levels). It is
important that `x`, `.default` and the columns of `lookup_table` are all
of the same type (specifically of the same
[`base::mode()`](https://rdrr.io/r/base/mode.html)). If the lookup table
is specified as a `vector` or `list`, only the `character` variables are
supported, because `name(lookup_table)` is always of mode `character`.

Original values are returned if they are not found in the lookup table.
Alternatively, a `.default` can be specified for values that are not
found. Note that it is possible to specify `NA` as one of the keys to
look up NA values (only when using a `data.frame` as lookup table).

Any names or attributes of x are preserved.

The `lookuper()` function returns *a function* equivalent to the
`lookup()` function, except that instead of taking a lookup table as an
argument, the lookup table is embedded in the function itself.

This can be very useful, in particular when using the lookup function as
an argument to other functions that expect a function which maps
`character`-\>`character` (or other data types), but do not offer a good
way to pass additional arguments to that function.

## Usage

``` r
lookup(x, lookup_table, ..., .default = x)

lookuper(lookup_table, ..., .default = NULL)
```

## Arguments

- x:

  A vector whose elements are to be looked up.

- lookup_table:

  The lookup table to use.

- ...:

  Reserved for future use.

- .default:

  If a value is not found in the lookup table, the value will be taken
  from `.default`. This must be a vector of the same mode as x, and
  either of length 1 or the same length as x. Useful values include `x`
  (the default setting), `NA`, or `""` (an empty string). Specifying
  `.default = NULL` implies that `x` will be used for missing values.

## Value

The `lookup()` function returns a vector based on `x`, with values
replaced with the lookup values from `lookup_table`. Any values not
found in the lookup table are taken from `.default`.

The `lookuper()` function returns *a function* that takes vectors as its
argument `x`, and returns either the corresponding values from the
underlying lookup table, or the original values from x for those
elements that are not found in the lookup table (or looks them up from
the `default`).

## Examples

``` r
fruit_lookup_vector <- c(a = "Apple", b = "Banana", c = "Cherry")
lookup(letters[1:5], fruit_lookup_vector)
#> [1] "Apple"  "Banana" "Cherry" "d"      "e"     
lookup(letters[1:5], fruit_lookup_vector, .default = NA)
#> [1] "Apple"  "Banana" "Cherry" NA       NA      

mtcars_lookup_data_frame <- data.frame(
  name = c("mpg", "hp", "wt"),
  value = c("Miles/(US) gallon", "Gross horsepower", "Weight (1000 lbs)"))
lookup(names(mtcars), mtcars_lookup_data_frame)
#>  [1] "Miles/(US) gallon" "cyl"               "disp"             
#>  [4] "Gross horsepower"  "drat"              "Weight (1000 lbs)"
#>  [7] "qsec"              "vs"                "am"               
#> [10] "gear"              "carb"             

# A more complex example, with numeric and NA values
numeric_lookup_table <- data.frame(
  key = c(1:5, NA), value = c(sqrt(1:5), 99999))
lookup(c(0:6, NA), numeric_lookup_table)
#> [1]     0.000000     1.000000     1.414214     1.732051     2.000000
#> [6]     2.236068     6.000000 99999.000000

lookup_fruits <- lookuper(list(a = "Apple", b = "Banana", c = "Cherry"))
lookup_fruits(letters[1:5])
#> [1] "Apple"  "Banana" "Cherry" "d"      "e"     
lookup_fruits_nomatch_na <-
  lookuper(list(a = "Apple", b = "Banana", c = "Cherry"), .default = NA)
lookup_fruits_nomatch_na(letters[1:5])
#> [1] "Apple"  "Banana" "Cherry" NA       NA      
```
