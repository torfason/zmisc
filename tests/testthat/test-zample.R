

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
    set.seed(1); expect_equal(zample(1:10,0),        numeric()   )
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



