
test_that("LHS can match multiple values", {
  recode_tilde(1, 1:2 ~ 10)  |> expect_identical(10)
  recode_tilde(1:5, 2:4 ~ 0) |> expect_identical(c(1, 0, 0, 0, 5))
  recode_tilde(1, 1:2 ~ "x") |> expect_error("Can't combine .*")
  recode_tilde(1, 1:2 ~ "x", .default = "Z") |> expect_identical("x")
})

test_that("LHS can match special values", {
  recode_tilde(NaN, NaN ~ 1)         |> expect_identical(1)
  recode_tilde(c("x", NA), NA ~ "y") |> expect_identical(c("x", "y"))
})

test_that("RHS is recycled to match x", {
  x <- 1:3
  recode_tilde(x, c(1, 3) ~ x * 2, .default = NULL) |> expect_identical(c(2, NA, 6))
  recode_tilde(x, c(1, 3) ~ x * 2, .default = NA)   |> expect_identical(c(2, NA, 6))
  recode_tilde(x, c(1, 3) ~ x * 2)                  |> expect_identical(c(2, 2, 6))
})

test_that("`NULL` values in `...` are dropped", {
  recode_tilde(1:2, 1 ~ 10, NULL, 2 ~ 20, NULL) |> expect_identical(c(10, 20))
})

test_that("passing no recoding formulas is OK", {
  recode_tilde(1)       |> expect_identical(1)
  recode_tilde(1, NULL) |> expect_identical(1)
})

test_that("passes through `.default` correctly", {
  recode_tilde(1,   3 ~ 1,   .default = 2)   |> expect_identical(2)
  recode_tilde(1:5, 6 ~ 1,   .default = 2)   |> expect_identical(rep(2, 5))
  recode_tilde(1:5, 6 ~ 1:5, .default = 2:6) |> expect_identical(2:6)
})

test_that("`.default` is part of common type computation", {
  recode_tilde(1, 1 ~ 1L, .default = 2)   |> expect_identical(1)
  recode_tilde(1, 1 ~ 1L, .default = "x") |> expect_snapshot(error = TRUE)
})

test_that("passes through `.ptype` correctly", {
  recode_tilde(1, 1 ~ 1, .ptype = integer()) |> expect_identical(1L)
})

test_that("`NULL` formula element throws meaningful error", {
  recode_tilde(1, 1 ~ NULL) |> expect_snapshot(error = TRUE)
  recode_tilde(1, NULL ~ 1) |> expect_snapshot(error = TRUE)
})

test_that("throws chained errors when formula evaluation fails", {
  recode_tilde(1, 1 ~ 2, 3 ~ stop("oh no!")) |> expect_snapshot(error = TRUE)
  recode_tilde(1, 1 ~ 2, stop("oh no!") ~ 4) |> expect_snapshot(error = TRUE)
})
