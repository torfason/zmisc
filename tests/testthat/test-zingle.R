
#### Test zingle() ####
test_that("zingle works", {

  # Check that if all equal, return first item
  expect_equal( zingle(rep(10,10)), 10 )
  expect_equal( zingle(rep("a",20)), "a" )
  expect_equal( zingle(as.factor(rep("a",20))), as.factor("a") )

  # Check that if not all equal, an error is thrown
  expect_error( zingle(1:2) )

  # Check that NAs behave as expected with na.rm
  expect_equal( zingle(c( 1, 1,NA, 1, 1), na.rm=TRUE),   1 )
  expect_equal( zingle(c(NA,NA,NA, 1,NA), na.rm=TRUE),   1 )
  expect_equal( zingle(c(NA,NA,NA,NA,NA), na.rm=TRUE),   NA )

  # Check that without na.rm=TRUE, NAs cause an error
  expect_error( zingle(c( 1, 1,NA, 1, 1)) )
  expect_error( zingle(c(NA,NA,NA, 1,NA)) )
  expect_error( zingle(c(NA,NA,NA,NA,NA)) )

  # Check that the result never has names
  expect_named( zingle(c(a=1,b=1)), NULL)
})
