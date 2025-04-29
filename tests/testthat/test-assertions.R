test_that("Basic acceptable assertions pass", {

  # Vars
  date_11 <- as.Date("1111-11-11")
  date_12 <- as.Date("1111-11-12")

  # Single-length assertions pass
  assert_flag(TRUE)       |> expect_equal(TRUE)
  assert_string("11")     |> expect_equal("11")
  assert_number(11)       |> expect_equal(11)
  assert_inumber(11L)     |> expect_equal(11L)
  assert_dnumber(11)      |> expect_equal(11)
  assert_int(11L)         |> expect_equal(11L)
  assert_count(11)        |> expect_equal(11)
  assert_day(date_11)     |> expect_equal(date_11)

  # Two_length assertions fail
  assert_flag(c(FALSE,TRUE))       |> expect_error("Must have length 1")
  assert_string(c("11", "12"))     |> expect_error("Must have length 1")
  assert_number(c(11,12))          |> expect_error("Must have length 1")
  assert_inumber(c(11L,12L))       |> expect_error("Must have length 1")
  assert_dnumber(c(11,12))         |> expect_error("Must have length 1")
  assert_int(c(11L,12L))           |> expect_error("Must have length 1")
  assert_count(c(11L,12L))         |> expect_error("Must have length 1")

  skip("Must test assert_day(), assert_naturalish(), and more ...")
  assert_day(c(date_11, date_12))  |> expect_error("Must have length 1")

  assert_naturalish(c(11L,12L))

})
