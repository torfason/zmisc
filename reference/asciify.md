# Convert non-ASCII characters to their ASCII equivalents

This function replaces non-ASCII characters in a string with their ASCII
equivalents. It supports a range of European non-ASCII characters,
including Icelandic, Swedish, Norwegian, Danish, Finnish, German,
Estonian, Latvian, Lithuanian, Polish, Hungarian, Slovenian, Czech,
Slovak, Maltese, Romanian, Albanian, and Croatian.

## Usage

``` r
asciify(x, verify = TRUE)
```

## Arguments

- x:

  A character vector to be processed.

- verify:

  A logical value indicating whether to verify that the result is ASCII.
  Defaults to `TRUE`. If `FALSE`, the function will not check that the
  result is ASCII and it may return non-ASCII characters.

## Value

A character vector with non-ASCII characters replaced by their ASCII
equivalents.

## Examples

``` r
asciify("Jón Þór Birgisson") # "Jon Thor Birgisson"
#> [1] "Jon Thor Birgisson"
asciify("förståndshandikapp") # "forstandshandikapp"
#> [1] "forstandshandikapp"
asciify("Viðareiði") # "Vidareidi"
#> [1] "Vidareidi"
asciify("übermensch") # "uebermensch"
#> [1] "uebermensch"
asciify("Jürgen Klopp") # "Juergen Klopp"
#> [1] "Juergen Klopp"
asciify("rõõmsameelsus") # "roomsameelsus"
#> [1] "roomsameelsus"
asciify("Mężczyzna") # "Mezczyzna"
#> [1] "Mezczyzna"
asciify("Škoda") # "Skoda"
#> [1] "Skoda"
```
