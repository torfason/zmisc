
test_that("There are no spelling errors", {
  skip_on_cran()
  if (!file.exists(here::here("DESCRIPTION")))
    skip("Skipping, this only runs through testthat tests.")

  # The actual test if we are not skipping it
  extract2 <- .Primitive("[[")
  spelling::spell_check_package(here::here()) |>
    extract2("word") |>
    expect_equal(character())
})
