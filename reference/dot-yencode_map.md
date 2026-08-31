# Compile an encoding table for yencode

Compile an encoding table for yencode

## Usage

``` r
.yencode_map(escape = "%", whitelist = c("._~-", "][!$&'()*+,;=:/?@#"))
```

## Arguments

- escape:

  The escape character to use.

- whitelist:

  Any characters that should not be escaped.

## Value

A list with the byte lookup table and the multi-byte whitelist entries
that need restoring after the byte pass.
