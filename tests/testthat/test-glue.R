
test_that("glue_vector() works", {
  ltrs <- letters[1:3]
  LTRS <- LETTERS[1:3]
  ltrs |>
    glue_vector("Letters include {.} and {LTRS}") |>
    expect_snapshot()
})
