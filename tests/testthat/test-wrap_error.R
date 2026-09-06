test_that("wrap_error() checks its arguments and prints the message", {
  e <- simpleCondition("something went wrong")

  expect_output(wrap_error(e), "#E>")
  expect_output(wrap_error(e, wrap = 20), "#E>")

  # wrap must be a positive whole number
  expect_error(wrap_error(e, wrap = 0), "not >= 1")
  expect_error(wrap_error(e, wrap = 2.5), "integerish")
  expect_error(wrap_error(e, wrap = c(10, 20)), "Must have length 1")

  expect_error(wrap_error("not a condition"), "Must inherit from class 'condition'")
})
