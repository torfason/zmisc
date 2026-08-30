
# Revert to e2 temporarily, to ensure reported regressions are not due to
# testthat changes but only due to new implementation of yencode() and friends
local_edition(2)


# Known inputs (and outputs below)
inputs <-
  c("A",
    "Ö",
    " ",                       # Space
    "#",                       # Must check well for hash
    "%",                       # Must check well for percentage
    "中国",                    # Multibyte
    "%20#20Z20A20",            # Ugly string, a space encoded using different escapes
    "LF\nCR-LF\r\nCR\r.",      # Ugly multiline string, will overwrite itself with cat
    "Hello",                   # Plain English
    "ÁÐÉÍÓÚÞÆÖáðéíóúþæö",      # Icelandic characters
    "ÑÁÉÍÓÚÜñáéíóúü",          # Spanish characters
    ".-_~",                    # Control characters never encoded by standard URLencode()
    "/!#$&/()=[]),;:+*?'")     # Control characters excluded by default - KEEP LAST

# Outputs of default URLencode() call
outputs_urlencode_default <-
  c("A",
    "%C3%96",
    "%20",
    "#",
    "%25",
    "%E4%B8%AD%E5%9B%BD",
    "%2520#20Z20A20",
    "LF%0ACR-LF%0D%0ACR%0D.",
    "Hello",
    "%C3%81%C3%90%C3%89%C3%8D%C3%93%C3%9A%C3%9E%C3%86%C3%96%C3%A1%C3%B0%C3%A9%C3%AD%C3%B3%C3%BA%C3%BE%C3%A6%C3%B6",
    "%C3%91%C3%81%C3%89%C3%8D%C3%93%C3%9A%C3%9C%C3%B1%C3%A1%C3%A9%C3%AD%C3%B3%C3%BA%C3%BC",
    ".-_~",
    "/!#$&/()=[]),;:+*?'"
    )

# The last string gets fully encoded if reserved is specified as true
outputs_urlencode_reserved_true <- outputs_urlencode_default
outputs_urlencode_reserved_true[4] <- "%23"              # With reserved true, hash is escaped
outputs_urlencode_reserved_true[7] <- "%2520%2320Z20A20" # Again, hash is escaped ...
outputs_urlencode_reserved_true[length(outputs_urlencode_reserved_true)] <-
  "%2F%21%23%24%26%2F%28%29%3D%5B%5D%29%2C%3B%3A%2B%2A%3F%27"


# The actual tests
test_that("yencode works", {

  x_default <- yencode(inputs)
  expect_equal(x_default, outputs_urlencode_default)
  expect_equal(ydecode(x_default), inputs)

  x_reserved_true <- yencode(inputs, whitelist = "._~-")
  expect_equal(x_reserved_true, outputs_urlencode_reserved_true)
  expect_equal(ydecode(x_reserved_true), inputs)

  # For filename escaping, we whitelist space, Icelandic, and Spanish
  hash_escaped_for_filenames <- yencode(inputs, escape = "#",
      whitelist = c(" ", "ÁÐÉÍÓÚÞÆÖáðéíóúþæö", "ÑÁÉÍÓÚÜñáéíóúü"))
  expect_equal(ydecode(hash_escaped_for_filenames, escape="#"), inputs)

})


test_that("yencode matches URLencode", {

  # yencode defaults should match URLencode defaults, with the exception that
  # yencode should NOT try to be smart about skipping encoding of strings
  # that look already encoded. It should be the user who deals with this.
  y_default <- yencode(inputs)
  u_default <- URLencode(inputs, repeated = TRUE)
  expect_equal(y_default, u_default)

  # Setting reserved==TRUE is equivalent to a shorter whitelist
  y_reserved <- yencode(inputs, whitelist = "._~-")
  u_reserved <- URLencode(inputs, reserved = TRUE, repeated = TRUE)
  expect_equal(y_reserved, u_reserved)

})


test_that("yencode is defensive", {

  # Whitelist must be a single character
  expect_error(yencode(letters, escape="#%",       whitelist = ""))
  expect_error(yencode(letters, escape=c("#","%"), whitelist = ""))


  # The escape character must not be in the whitelist
  expect_warning(yencode(letters, whitelist="%"))
  expect_warning(yencode(letters, escape="#"))
  expect_warning(yencode(letters, escape="1"))

})


test_that("yencode handles strange escapes", {

  # Specify what we expect for escape==Z
  expected_Z <- c(
"A", "ZC3Z96", "Z20", "Z23", "Z25", "ZE4ZB8ZADZE5Z9BZBD", "Z2520Z2320Z5A20A20", "LFZ0ACRZ2DLFZ0DZ0ACRZ0DZ2E", "Hello",
"ZC3Z81ZC3Z90ZC3Z89ZC3Z8DZC3Z93ZC3Z9AZC3Z9EZC3Z86ZC3Z96ZC3ZA1ZC3ZB0ZC3ZA9ZC3ZADZC3ZB3ZC3ZBAZC3ZBEZC3ZA6ZC3ZB6",
"ZC3Z91ZC3Z81ZC3Z89ZC3Z8DZC3Z93ZC3Z9AZC3Z9CZC3ZB1ZC3ZA1ZC3ZA9ZC3ZADZC3ZB3ZC3ZBAZC3ZBC",
"Z2EZ2DZ5FZ7E", "Z2FZ21Z23Z24Z26Z2FZ28Z29Z3DZ5BZ5DZ29Z2CZ3BZ3AZ2BZ2AZ3FZ27")

  # Specify what we expect for escape==A
  expected_A <- c(
"A41", "AC3A96", "A20", "A23", "A25", "AE4AB8AADAE5A9BABD", "A2520A2320Z20A4120", "LFA0ACRA2DLFA0DA0ACRA0DA2E", "Hello",
"AC3A81AC3A90AC3A89AC3A8DAC3A93AC3A9AAC3A9EAC3A86AC3A96AC3AA1AC3AB0AC3AA9AC3AADAC3AB3AC3ABAAC3ABEAC3AA6AC3AB6",
"AC3A91AC3A81AC3A89AC3A8DAC3A93AC3A9AAC3A9CAC3AB1AC3AA1AC3AA9AC3AADAC3AB3AC3ABAAC3ABC",
"A2EA2DA5FA7E", "A2FA21A23A24A26A2FA28A29A3DA5BA5DA29A2CA3BA3AA2BA2AA3FA27")

  x_Z <- expect_warning(yencode(inputs, escape="Z", whitelist=""))
  x_A <- expect_warning(yencode(inputs, escape="A", whitelist=""))

  # Test that the encoded output is what we expected
  expect_equal(x_Z, expected_Z)
  expect_equal(x_A, expected_A)

  # Test that the encodings work correctly for the round-trip
  expect_equal(ydecode(x_Z, "Z"), inputs)
  expect_equal(ydecode(x_A, "A"), inputs)

  # Multibyte characters do not work as escape prefixes, so they
  # should be explicitly disallowed by the functions.
  expect_error(yencode(inputs, escape="中", whitelist=""))
  expect_error(ydecode(x_中, "中"))

})


test_that("yencoder and decoder work", {
  f_Z <- expect_warning(yencoder("Z", whitelist=""))
  x_Z <- expect_warning(yencode(inputs, escape="Z", whitelist=""))
  x_Z_encoder <- f_Z(inputs)
  expect_equal(x_Z_encoder, x_Z)

  f_perc <- ydecoder("%")
  x_perc <- ydecode(outputs_urlencode_default, escape="%")
  x_perc_decoder <- f_perc(outputs_urlencode_default)
  expect_equal(x_perc_decoder, x_perc)
})
