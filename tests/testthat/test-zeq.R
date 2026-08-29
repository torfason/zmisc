
#### Test zeq() ####
test_that("zeq works", {

  # Positive intervals should equal seq
  expect_equal( zeq(1,2),      seq(1,2)    )
  expect_equal( zeq(1,10),     seq(1,10)   )
  expect_equal( zeq(5,10),     seq(5,10)   )
  expect_equal( zeq(20,20),    seq(20,20)  )

  # Identity should have length one
  expect_equal( zeq(1,1),           1     )

  # End one less than start gives empty sequence
  expect_equal( zeq(1,0),    integer(0)  )

  # End lower than start by two or more gives error
  expect_error( zeq(2,0) )
})

test_that("zeq returns integers, like seq() does", {

  # Integerish doubles should still yield an integer sequence
  expect_identical( zeq(11, 15),  11:15      )
  expect_identical( zeq(11L, 15L), 11:15     )
  expect_identical( zeq(11, 10),  integer(0) )

  expect_type( zeq(11, 15), "integer" )
  expect_type( zeq(11, 10), "integer" )
})

test_that("zeq requires single, non-NA, integerish bounds", {

  # Vectors of length other than one must error, not recycle silently
  expect_error( zeq(c(1,2), 5) )
  expect_error( zeq(1, c(5,6)) )
  expect_error( zeq(integer(0), 5) )
  expect_error( zeq(1, integer(0)) )

  # NA bounds must error
  expect_error( zeq(NA, 5) )
  expect_error( zeq(1, NA) )
  expect_error( zeq(NA_integer_, 5) )

  # Non-integerish bounds must error
  expect_error( zeq(1.5, 5)  )
  expect_error( zeq(1, 5.5)  )
  expect_error( zeq("1", 5)  )
  expect_error( zeq(1, "5")  )
  expect_error( zeq(NULL, 5) )
})
