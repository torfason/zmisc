
#### Test zingle() ####
test_that("zingle works", {

  # Check that if all equal, return first item
  expect_equal( zingle(rep(10,10)), 10 )
  expect_equal( zingle(rep("a",20)), "a" )
  expect_equal( zingle(as.factor(rep("a",20))), as.factor("a") )

  # Check that if not all equal, an error is thrown
  expect_error( zingle(1:2) )

  # Check that NAs behave as expected with na.ok.<all/partial>
  expect_equal( zingle(c( 1, 1,NA, 1, 1), na.ok.partial = TRUE),   1 )
  expect_equal( zingle(c(NA,NA,NA, 1,NA), na.ok.partial = TRUE),   1 )
  expect_equal( zingle(c(NA,NA,NA,NA,NA), na.ok.all = TRUE),   NA )

  # Check that without na.ok.<all/partial> NAs cause an error
  expect_error( zingle(c( 1, 1,NA, 1, 1)) )
  expect_error( zingle(c(NA,NA,NA, 1,NA)) )
  expect_error( zingle(c(NA,NA,NA,NA,NA)) )

  # Check that the result never has names
  expect_named( zingle(c(a=1,b=1)), NULL)
})

#### Test zingle() ####
test_that("zingle handles various unusual values", {

  # Define inputs
  nan   <- rep(NaN, 3)
  na    <- rep(NA, 3)
  inf   <- rep(Inf, 3)
  nginf <- rep(-Inf, 3)
  empty <- character(0)
  null  <- NULL
  lst   <- list(3, 3, 3)
  m     <- matrix(1)

  # The following should pass and return correct values
  zingle(nan, nan.ok.all = TRUE) |>
    expect_length(1) |>
    is.nan() |> expect_true()
  zingle(na, na.ok.all = TRUE) |>
    expect_length(1) |>
    is.na() |> expect_true()
  zingle(inf) |>
    expect_length(1) |>
    expect_equal(Inf)
  zingle(nginf) |>
    expect_length(1) |>
    expect_equal(-Inf)

  # NA is not ok unless na.ok.all or na.ok.partial
  zingle(na) |>
    expect_error()

  # NaN is NOT ok unless nan.ok.all
  zingle(nan) |>
    expect_error()

  # Empty is NOT ok unless empty.ok
  zingle(empty) |>
    expect_error()
  zingle(empty, empty.ok = TRUE) |>
    expect_length(1) |>
    is.na() |> expect_true()

  # rm.na = TRUE on an NA-only vector should also fail unless empty.ok = TRUE
  zingle(na) |>
    expect_error()
  zingle(na, na.ok.all = TRUE, empty.ok = TRUE) |>
    expect_length(1) |>
    is.na() |> expect_true()

  # Null is NOT ok
  zingle(null) |>
    expect_error()

  # Lists are NOT ok
  zingle(lst) |>
    expect_error()

  # Matrices are NOT ok
  zingle(m) |>
    expect_error()
})


test_that("zingle handles missing values the way mean() does", {

  # mean() returns NA rather than erroring for these, and so should zingle()
  expect_equal( zingle(c( 1, 1,NA, 1, 1), na.ok.partial = TRUE), 1 )
  expect_equal( zingle(c(NA,NA,NA, 1,NA), na.ok.partial = TRUE), 1 )
  expect_equal( zingle(c(NA,NA,NA,NA,NA), na.ok.all = TRUE), NA )
  expect_equal( zingle(c("a","a",NA), na.ok.partial = TRUE), "a" )

  # An empty vector has no single value either, so it too is NA, not an error
  expect_equal( zingle(numeric(0), empty.ok = TRUE),   NA_real_ )
  expect_equal( zingle(character(0), empty.ok = TRUE), NA_character_ )
  expect_equal( zingle(logical(0), empty.ok = TRUE),   NA )

  # The equality check still applies to whichever values are present, so a
  # genuine disagreement is an error even when an NA is in the mix
  expect_error( zingle(c("a","b",NA)) )
  expect_error( zingle(c(1,2,NA), na.ok.partial = TRUE) )
})

test_that("zingle preserves the type and class of the NA it returns", {

  # A bare NA of the wrong type would silently change the type of a column
  expect_identical( zingle(character(0), empty.ok = TRUE),   NA_character_ )
  expect_identical( zingle(integer(0), empty.ok = TRUE),     NA_integer_   )
  expect_identical( zingle(double(0), empty.ok = TRUE),      NA_real_      )
  expect_identical( zingle(c(NA_real_,NA_real_), na.ok.all = TRUE), NA_real_ )

  expect_identical( zingle(c("a",NA), na.ok.partial = TRUE),   "a")

  expect_error( zingle(c(NA_real_,NA_real_)))
  expect_error( zingle(c("a",NA)))
  expect_error( zingle(c(NA_real_,NA_real_), na.ok.partial = TRUE))
  expect_error( zingle(c("a",NA), na.ok.all = TRUE))

  # Classed vectors keep their class as well
  f <- factor(c("a","a"), levels = c("a","b"))
  expect_s3_class( zingle(f), "factor" )
  expect_identical( levels(zingle(f)), c("a","b") )

  f.na <- factor(c("a","a",NA), levels = c("a","b"))
  expect_error(  zingle(f.na) )

  d <- as.Date(c("2020-01-01", NA))
  expect_s3_class( zingle(d, na.ok.partial = TRUE), "Date" )
  expect_equal( zingle(d, na.ok.partial = TRUE), as.Date("2020-01-01") )
  expect_error( zingle(as.Date(character(0))) )
  expect_s3_class( zingle(as.Date(character(0)), empty.ok = TRUE), "Date" )
})

test_that("zingle ignores names, including on the NA path", {
  expect_named( zingle(c(a=1,b=1)),  NULL )
  expect_named( zingle(c(a=1,b=NA), na.ok.partial = TRUE), NULL )
  expect_named( zingle(c(a=NA,b=NA), na.ok.all = TRUE), NULL )
})
