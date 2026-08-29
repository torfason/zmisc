

#### Test zample() ####
test_that("zample works", {

    # Expected first ten samples with seed at 1
    x = c(9L, 4L, 7L, 1L, 2L, 5L, 3L, 10L, 6L, 8L)
    s = c("y", "d", "g", "a", "b", "k", "n", "r", "w", "j", "f", "t",
          "q", "x", "i", "e", "u", "l", "s", "p", "o", "m", "v", "z",
          "c", "h")

    # Basic operations, numeric
    set.seed(1); expect_equal(zample(1:10),   x    )
    set.seed(1); expect_equal(zample(1:10,1), x[1] )
    set.seed(1); expect_equal(zample(1:10,2), x[1:2] )

    set.seed(1); expect_equal(zample(letters),   s    )
    set.seed(1); expect_equal(zample(letters,1), s[1] )
    set.seed(1); expect_equal(zample(letters,2), s[1:2] )

    # Sampling from a vector of length on should only
    # yield a single result (not treat as number of samples)
    set.seed(1); expect_equal(zample(10),      10     )
    set.seed(1); expect_equal(zample("a"),    "a"     )

    # Test that zero-length sampling works
    set.seed(1); expect_equal(zample(1:10,0),        integer()   )
    set.seed(1); expect_equal(zample(letters,0),     character() )
    set.seed(1); expect_equal(zample(numeric()),     numeric()   )
    set.seed(1); expect_equal(zample(character()),   character() )
    set.seed(1); expect_equal(zample(numeric(),0),   numeric()   )
    set.seed(1); expect_equal(zample(character(),0), character() )

    # But sampling more than zero from zero-length vector should not work
    expect_error(zample(numeric(),   1))
    expect_error(zample(character(), 1))

    # Zample should not try to sample from data.frames
    expect_error(zample(cars))
    expect_error(zample(cars, 4))
})

test_that("zample preserves the class of x", {

  # Classed vectors keep their class, both when sampled and when empty
  f <- factor(c("a", "b", "c"), levels = c("a", "b", "c", "d"))
  set.seed(1); expect_s3_class( zample(f), "factor" )
  set.seed(1); expect_identical( levels(zample(f)), levels(f) )
  expect_identical( zample(f, 0), f[0] )
  expect_identical( zample(f[0]), f[0] )

  d <- as.Date(c("2020-01-01", "2020-06-01"))
  set.seed(1); expect_s3_class( zample(d), "Date" )
  expect_identical( zample(d, 0), d[0] )
  expect_identical( zample(as.Date(character(0))), as.Date(character(0)) )

  # Zero-length input of a bare type keeps its own type, not just its mode
  expect_identical( zample(integer(0)),   integer(0)   )
  expect_identical( zample(character(0)), character(0) )
  expect_identical( zample(logical(0)),   logical(0)   )
  expect_identical( zample(complex(0)),   complex(0)   )
})

test_that("zample preserves names and honours prob", {

  # Names travel with the elements they belong to
  v <- c(a = 1, b = 2, c = 3)
  set.seed(1); r <- zample(v)
  expect_setequal( names(r), names(v) )
  expect_equal( r, v[names(r)] )

  # A degenerate prob vector makes the outcome deterministic
  expect_equal( zample(1:3, size = 4, replace = TRUE, prob = c(0, 1, 0)),
                rep(2L, 4) )

  # prob must still be validated by sample.int()
  expect_error( zample(1:3, prob = c(1, 1)) )
})
