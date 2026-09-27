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

## Details

Letters and digits are never escaped. Other characters are escaped
unless they appear in `whitelist`, which may include multi-byte
characters. The escape character is removed from the whitelist, with a
warning, if present, and must itself be a single ASCII character.

`yencode()` escapes the UTF-8 representation of `string`, whatever its
declared encoding, so the same text always gives the same result.
`ydecode()` returns strings marked as UTF-8, and raises an error if an
escape sequence is malformed or the decoded bytes are not valid UTF-8.
