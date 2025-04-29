test_that("Basic scalar assertion functions work", {

  # Vars
  date_11 <- as.Date("1111-11-11")
  date_12 <- as.Date("1111-11-12")

  # Single-length assertions pass
  assert_flag(TRUE)       |> expect_identical(TRUE)
  assert_string("11")     |> expect_identical("11")
  assert_number(11)       |> expect_identical(11)
  assert_inumber(11L)     |> expect_identical(11L)
  assert_dnumber(11)      |> expect_identical(11)
  assert_int(11L)         |> expect_identical(11L)
  assert_count(11)        |> expect_identical(11)
  assert_day(date_11)     |> expect_identical(date_11)

  # Negative count should fail
  assert_count(-1) |> expect_error("Must be >= 0")

  # Two_length assertions fail
  assert_flag(c(FALSE, TRUE))      |> expect_error("Must have length 1")
  assert_string(c("11", "12"))     |> expect_error("Must have length 1")
  assert_number(c(11,12))          |> expect_error("Must have length 1")
  assert_inumber(c(11L,12L))       |> expect_error("Must have length 1")
  assert_dnumber(c(11,12))         |> expect_error("Must have length 1")
  assert_int(c(11L,12L))           |> expect_error("Must have length 1")
  assert_count(c(11L,12L))         |> expect_error("Must have length 1")
  assert_day(c(date_11, date_12))  |> expect_error("Must have length 1")

  # NA assertions fail
  assert_flag(NA)     |> expect_error("May not be NA")
  assert_string(NA)   |> expect_error("May not be NA")
  assert_number(NA)   |> expect_error("May not be NA")
  assert_inumber(NA)  |> expect_error("May not be NA")
  assert_dnumber(NA)  |> expect_error("May not be NA")
  assert_int(NA)      |> expect_error("May not be NA")
  assert_count(NA)    |> expect_error("May not be NA")
  assert_day(NA)      |> expect_error("May not be NA")

  # NULL assertions fail
  assert_flag(NULL)         |> expect_error("Must be of type .* not 'NULL'")
  assert_string(NULL)       |> expect_error("Must be of type .* not 'NULL'")
  assert_number(NULL)       |> expect_error("Must be of type .* not 'NULL'")
  assert_inumber(NULL)      |> expect_error("Must be of type .* not 'NULL'")
  assert_dnumber(NULL)      |> expect_error("Must be of type .* not 'NULL'")
  assert_int(NULL)          |> expect_error("Must be of type .* not 'NULL'")
  assert_count(NULL)        |> expect_error("Must be of type .* not 'NULL'")
  assert_day(NULL)          |> expect_error("Must be of type .* not 'NULL'")

})


test_that("Basic vector assertion functions work", {

  # Vars
  date_11 <- as.Date("1111-11-11")
  date_12 <- as.Date("1111-11-12")
  date_NA <- as.Date(NA)  # Note: Unclassed dates do not pass assert_date()

  # Two_length assertions fail
  assert_logical(c(FALSE, TRUE))    |> expect_identical(c(FALSE, TRUE))
  assert_character(c("11", "12"))   |> expect_identical(c("11", "12"))
  assert_numeric(c(11,12))          |> expect_identical(c(11,12))
  assert_integer(c(11L,12L))        |> expect_identical(c(11L,12L))
  assert_double(c(11,12))           |> expect_identical(c(11,12))
  assert_integerish(c(11L,12L))     |> expect_identical(c(11L,12L))
  assert_naturalish(c(11L,12L))     |> expect_identical(c(11L,12L))
  assert_date(c(date_11, date_12))  |> expect_identical(c(date_11, date_12))

  # Negative naturalish should fail
  assert_naturalish(-1) |> expect_error("Element 1 is not >= 0")

  # NA assertions PASS for vector assertions
  assert_logical(NA)       |> expect_identical(NA)
  assert_character(NA)     |> expect_identical(NA)
  assert_numeric(NA)       |> expect_identical(NA)
  assert_integer(NA)       |> expect_identical(NA)
  assert_double(NA)        |> expect_identical(NA)
  assert_integerish(NA)    |> expect_identical(NA)
  assert_naturalish(NA)    |> expect_identical(NA)
  assert_date(date_NA)     |> expect_identical(date_NA)

  # NULL assertions fail
  assert_logical(NULL)       |> expect_error("Must be of type 'logical', not 'NULL'")
  assert_character(NULL)     |> expect_error("Must be of type 'character', not 'NULL'")
  assert_numeric(NULL)       |> expect_error("Must be of type 'numeric', not 'NULL'")
  assert_integer(NULL)       |> expect_error("Must be of type 'integer', not 'NULL'")
  assert_double(NULL)        |> expect_error("Must be of type 'double', not 'NULL'")
  assert_integerish(NULL)    |> expect_error("Must be of type 'integerish', not 'NULL'")
  assert_naturalish(NULL)    |> expect_error("Must be of type 'naturalish', not 'NULL'")
  assert_date(NULL)          |> expect_error("Must be of class 'Date', not 'NULL'")

})
