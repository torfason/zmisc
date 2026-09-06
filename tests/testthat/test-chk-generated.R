# test_that("R/chk-generated.R is up to date with its generator", {
#   skip_on_cran()
#   skip_if_not_installed("tibble")
#   skip_if_not_installed("purrr")
#   gen <- test_path("..", "..", "data-raw", "generate-chk.R")
#   out <- test_path("..", "..", "R", "chk-generated.R")
#   skip_if_not(file.exists(gen) && file.exists(out), "generator not available")
#
#   env <- new.env()
#   suppressMessages(sys.source(gen, envir = env))
#   expect_identical(readLines(out), env$render_chk())
# })


test_that("the inlined fast path agrees with attrs_ok()", {
  cases <- list(
    1:3, c(a = 1, b = 2), "x", TRUE, NULL, NA,
    matrix(1:4, 2), factor("a"), as.Date("2000-01-01"),
    structure(1:3, label = "n"), structure(1:3, class = "myclass")
  )
  for (x in cases) {
    inlined <- is.vector(x, "any") || is.null(x)
    expect_identical(inlined, attrs_ok(x, "names"), info = deparse1(x))
  }
})


test_that("attr.ok accepts a list, none or any", {
  lab <- structure(1:3, label = "n")

  expect_identical(chk_integer(lab, attr.ok = TRUE), lab)
  expect_identical(chk_integer(lab, attr.ok = "label"), lab)
  expect_error(chk_integer(lab, attr.ok = "names"), "Must not have attributes: label")
  expect_error(chk_integer(lab, attr.ok = FALSE), "Must not have attributes: label")

  named <- c(a = 1, b = 2)
  expect_identical(chk_numeric(named, attr.ok = "names"), named)
  expect_error(chk_numeric(named, attr.ok = FALSE), "Must not have attributes: names")

  # dim and class keep their own wording
  expect_error(chk_numeric(matrix(1:4, 2), attr.ok = "names"), "Must not have a dim attribute")
  expect_error(chk_integer(factor("a"), attr.ok = "levels"), "Must not have a class attribute")

  # a bare vector passes every setting
  for (a in list("names", FALSE, TRUE, character())) {
    expect_identical(chk_numeric(1.5, attr.ok = a), 1.5)
  }
})


test_that("NULL is decided by null.ok alone, on both paths", {
  # The attribute contract is vacuous for NULL, so a chk_*() that accepts
  # null.ok must accept NULL whether or not other arguments were supplied.
  nms <- grep("^chk_", getNamespaceExports("zmisc"), value = TRUE)
  for (nm in nms) {
    f <- get(nm, envir = asNamespace("zmisc"))
    if (!"null.ok" %in% names(formals(f))) next
    expect_identical(f(NULL, null.ok = TRUE), NULL, info = nm)
  }
})

test_that("Error includes var name", {

  # Symbols, extractions and calls are deparsed as written
  myvar <- "not a number"
  chk_numeric(myvar) |> expect_error("Assertion on `myvar` failed")

  # Test how it works inside functions
  f <- function(myparam) chk_string(myparam)
  f(letters) |> expect_error("Assertion on `myparam` failed")


})
