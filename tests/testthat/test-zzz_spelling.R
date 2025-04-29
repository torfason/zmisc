
test_that("There are no spelling errors", {
  extract2 <- .Primitive("[[")
  if (!file.exists(here::here("DESCRIPTION")))
    skip("Skipping, this only runs through testthat tests.")
  spelling::spell_check_package(here::here()) |>
    extract2("word") |>
    expect_equal(character())
})
