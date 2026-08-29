test_that("Basic scalar assertion functions work", {

  # Vars
  date_11 <- as.Date("1111-11-11")
  date_12 <- as.Date("1111-11-12")

  # Single-length assertions pass
  chk_flag(TRUE)       |> expect_identical(TRUE)
  chk_string("11")     |> expect_identical("11")
  chk_number(11)       |> expect_identical(11)
  chk_inumber(11L)     |> expect_identical(11L)
  chk_dnumber(11)      |> expect_identical(11)
  chk_znumber(11L)     |> expect_identical(11L)
  chk_count(11)        |> expect_identical(11)
  chk_day(date_11)     |> expect_identical(date_11)

  # Negative count should fail
  chk_count(-1) |> expect_error("Must be >= 0")

  # Two_length assertions fail
  chk_flag(c(FALSE, TRUE))      |> expect_error("Must have length 1")
  chk_string(c("11", "12"))     |> expect_error("Must have length 1")
  chk_number(c(11,12))          |> expect_error("Must have length 1")
  chk_inumber(c(11L,12L))       |> expect_error("Must have length 1")
  chk_dnumber(c(11,12))         |> expect_error("Must have length 1")
  chk_znumber(c(11L,12L))       |> expect_error("Must have length 1")
  chk_count(c(11L,12L))         |> expect_error("Must have length 1")
  chk_day(c(date_11, date_12))  |> expect_error("Must have length 1")

  # NA assertions fail
  chk_flag(NA)     |> expect_error("May not be NA")
  chk_string(NA)   |> expect_error("May not be NA")
  chk_number(NA)   |> expect_error("May not be NA")
  chk_inumber(NA)  |> expect_error("May not be NA")
  chk_dnumber(NA)  |> expect_error("May not be NA")
  chk_znumber(NA)  |> expect_error("May not be NA")
  chk_count(NA)    |> expect_error("May not be NA")
  chk_day(NA)      |> expect_error("May not be NA")

  # NULL assertions fail
  chk_flag(NULL)         |> expect_error("Must be of type .* not 'NULL'")
  chk_string(NULL)       |> expect_error("Must be of type .* not 'NULL'")
  chk_number(NULL)       |> expect_error("Must be of type .* not 'NULL'")
  chk_inumber(NULL)      |> expect_error("Must be of type .* not 'NULL'")
  chk_dnumber(NULL)      |> expect_error("Must be of type .* not 'NULL'")
  chk_znumber(NULL)      |> expect_error("Must be of type .* not 'NULL'")
  chk_count(NULL)        |> expect_error("Must be of type .* not 'NULL'")
  chk_day(NULL)          |> expect_error("Must be of type .* not 'NULL'")

})


test_that("Basic vector assertion functions work", {

  # Vars
  date_11 <- as.Date("1111-11-11")
  date_12 <- as.Date("1111-11-12")
  date_NA <- as.Date(NA)  # Note: Unclassed dates do not pass chk_date()

  # Two_length assertions fail
  chk_logical(c(FALSE, TRUE))    |> expect_identical(c(FALSE, TRUE))
  chk_character(c("11", "12"))   |> expect_identical(c("11", "12"))
  chk_numeric(c(11,12))          |> expect_identical(c(11,12))
  chk_integer(c(11L,12L))        |> expect_identical(c(11L,12L))
  chk_double(c(11,12))           |> expect_identical(c(11,12))
  chk_integerish(c(11L,12L))     |> expect_identical(c(11L,12L))
  chk_naturalish(c(11L,12L))     |> expect_identical(c(11L,12L))
  chk_date(c(date_11, date_12))  |> expect_identical(c(date_11, date_12))

  # Negative naturalish should fail
  chk_naturalish(-1) |> expect_error("Element 1 is not >= 0")

  # NA assertions PASS for vector assertions
  chk_logical(NA)       |> expect_identical(NA)
  chk_character(NA)     |> expect_identical(NA)
  chk_numeric(NA)       |> expect_identical(NA)
  chk_integer(NA)       |> expect_identical(NA)
  chk_double(NA)        |> expect_identical(NA)
  chk_integerish(NA)    |> expect_identical(NA)
  chk_naturalish(NA)    |> expect_identical(NA)
  chk_date(date_NA)     |> expect_identical(date_NA)

  # NULL assertions fail
  chk_logical(NULL)       |> expect_error("Must be of type 'logical', not 'NULL'")
  chk_character(NULL)     |> expect_error("Must be of type 'character', not 'NULL'")
  chk_numeric(NULL)       |> expect_error("Must be of type 'numeric', not 'NULL'")
  chk_integer(NULL)       |> expect_error("Must be of type 'integer', not 'NULL'")
  chk_double(NULL)        |> expect_error("Must be of type 'double', not 'NULL'")
  chk_integerish(NULL)    |> expect_error("Must be of type 'integerish', not 'NULL'")
  chk_naturalish(NULL)    |> expect_error("Must be of type 'naturalish', not 'NULL'")
  chk_date(NULL)          |> expect_error("Must be of class 'Date', not 'NULL'")

})


test_that("POSIXct assertion functions work", {

  # Vars
  time_11 <- as.POSIXct("1111-11-11 11:11:11", tz = "UTC")
  time_12 <- as.POSIXct("1111-11-12 12:12:12", tz = "UTC")
  time_NA <- as.POSIXct(NA)

  # Single-length assertions pass for the scalar version
  chk_instant(time_11)  |> expect_identical(time_11)

  # Vectors pass the vector version, but not the scalar one
  chk_posixct(c(time_11, time_12)) |> expect_identical(c(time_11, time_12))
  chk_instant(c(time_11, time_12)) |> expect_error("Must have length 1")

  # NA is allowed for the vector version but not the scalar one, and the
  # scalar version reports it the same way chk_day() does
  chk_posixct(time_NA) |> expect_identical(time_NA)
  chk_instant(time_NA) |> expect_error("May not be NA")

  # NULL is reported by type, again in parallel with chk_day()
  chk_instant(NULL) |> expect_error("Must be of type 'instant', not 'NULL'")
  chk_posixct(NULL) |> expect_error("Must be of type 'POSIXct', not 'NULL'")

  # A Date is not a POSIXct, and neither is a bare number
  chk_instant(as.Date("1111-11-11")) |> expect_error("Must be of type 'POSIXct'")
  chk_posixct(as.Date("1111-11-11")) |> expect_error("Must be of type 'POSIXct'")
  chk_instant(11) |> expect_error("Must be of type 'POSIXct'")

  # Additional parameters are passed through to checkmate
  chk_posixct(c(time_11, time_12), len = 3) |> expect_error("Must have length 3")
})


test_that("qassert reports the reason for the failure", {

  # Passing assertions return their input invisibly
  qassert(11L, "I1")      |> expect_identical(11L)
  qassert(c(1,2,3), "N3") |> expect_identical(c(1,2,3))

  # qtest() reports only TRUE/FALSE, so a failure has to recover its message
  # from checkmate rather than passing the FALSE straight to abort()
  qassert(1:3, "S1") |> expect_error("Must be of class 'string'")
  qassert("a", "N1") |> expect_error("Must be of class 'numeric'")
  qassert(NULL, "N1") |> expect_error("Must be of class 'numeric'")

  # The failure must never be rlang complaining about abort()'s own argument
  qassert(1:3, "S1") |> expect_error("^(?!.*must be a character vector)", perl = TRUE)
})
