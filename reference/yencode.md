# Yet (another urlencode compatible) encoding scheme

Yet (another urlencode compatible) encoding scheme

## Usage

``` r
yencode(string, escape = "%", whitelist = c("._~-", "][!$&'()*+,;=:/?@#"))

yencoder(escape = "%", whitelist = c("._~-", "][!$&'()*+,;=:/?@#"))

ydecode(string, escape = "%")

ydecoder(escape = "%")
```

## Arguments

- string:

  The string to process.

- escape:

  The escape character to use.

- whitelist:

  Any characters that should not be escaped. See details.

## Value

The processed (encoded or decoded) string.
