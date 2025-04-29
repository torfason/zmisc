
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
  expect_equal( zeq(1,0),    numeric(0)  )

  # End lower than start by two or more gives error
  expect_error( zeq(2,0) )
})
