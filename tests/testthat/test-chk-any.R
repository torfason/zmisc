
# Tests for chk_any(), the composite assertion.
#
# The blocks follow the seams of the design rather than the parameter list,
# since chk_any() has no parameters: what passes and what is returned, what a
# combined failure says, what is deliberately not caught, and the suppression
# protocol that lets a failing assertion return instead of throwing.


test_that("chk_any() returns the first branch that passes", {

  s <- "a"
  n <- 3L

  chk_any(chk_string(s), chk_number(s))   |> expect_equal(s)
  chk_any(chk_string(n), chk_number(n))   |> expect_equal(n)
  chk_any(chk_number(n), chk_string(n))   |> expect_equal(n)

  # the value comes back invisibly, as it does from every other assertion
  expect_invisible(chk_any(chk_string(s)))

  # a passing branch short-circuits, so a later one is never evaluated
  seen <- FALSE
  chk_any(chk_string(s), chk_number({seen <<- TRUE; 1}))
  expect_false(seen)

  # the assertions are evaluated where they were written
  (function() {
    local_var <- "b"
    chk_any(chk_string(local_var), chk_number(local_var)) |> expect_equal("b")
  })()

})


test_that("chk_any() reports every failure when none pass", {

  b <- TRUE

  chk_any(chk_string(b), chk_number(b)) |>
    expect_error("none of the alternatives passed")
  chk_any(chk_string(b), chk_number(b)) |>
    expect_error("Must be of type 'string', not 'logical'")
  chk_any(chk_string(b), chk_number(b)) |>
    expect_error("Must be of type 'number', not 'logical'")

  # one expression asserted on throughout, so the bullets carry no labels
  chk_any(chk_string(b), chk_number(b)) |>
    expect_error("Assertion on `b` failed")

  # branches that disagree, so each bullet says which one it belongs to
  s <- "a"
  chk_any(chk_string(b), chk_number(s)) |>
    expect_error("`b`: Must be of type 'string'")
  chk_any(chk_string(b), chk_number(s)) |>
    expect_error("`s`: Must be of type 'number'")

  # a single branch is allowed, and reads like the assertion it wraps
  chk_any(chk_string(b)) |> expect_error("Must be of type 'string', not 'logical'")

})


test_that("chk_any() nests", {

  b <- TRUE
  n <- 3L

  # an inner chk_any() that passes settles the outer one
  chk_any(chk_any(chk_string(b), chk_number(n)), chk_flag(b)) |> expect_equal(n)

  # an inner one that fails does not throw, it lets the outer branch decide
  chk_any(chk_any(chk_string(b), chk_number(b)), chk_flag(b)) |> expect_true()

  # and when nothing passes, both levels are reported
  chk_any(chk_any(chk_string(b), chk_number(b)), chk_flag("q")) |>
    expect_error("Must be of type 'string', not 'logical'")
  chk_any(chk_any(chk_string(b), chk_number(b)), chk_flag("q")) |>
    expect_error("Must be of type 'logical flag', not 'character'")

})


test_that("chk_any() catches assertion failures and nothing else", {

  s <- "a"

  # a misspelled assertion, an argument that does not exist, and an object that
  # was never bound all travel, where tryCatch() would have swallowed them
  chk_any(chk_strng(s), chk_number(s))               |> expect_error("could not find function")
  chk_any(chk_string(s, nosucharg = 1), chk_flag(s)) |> expect_error("must be empty")
  chk_any(chk_string(nosuchobject), chk_number(1))   |> expect_error("not found")

  # an assertion one frame further down is not a branch, and still throws
  wrapper <- function(x) { chk_string(x); "wrapper returned" }
  chk_any(wrapper("b"), chk_number(1)) |> expect_equal("wrapper returned")
  chk_any(wrapper(1), chk_number(1))   |> expect_error("Must be of type 'string', not 'double'")

  # namespace qualified calls are branches like any other
  chk_any(zmisc::chk_string(1), zmisc::chk_number(1)) |> expect_equal(1)

})


test_that("chk_any() rejects arguments that cannot assert", {

  s <- "a"

  chk_any()                            |> expect_error("at least one assertion")
  chk_any(a = chk_string(s))           |> expect_error("must not be named")
  chk_any(s, chk_string(s))            |> expect_error("is not one")
  chk_any(chk_string(s), 42)           |> expect_error("is not one")

  # the case that guard is really for: the piped object lands where the first
  # assertion was expected, and would otherwise come back unchecked
  ("a" |> chk_any(chk_string(), chk_number())) |> expect_error("cannot be piped")

  # forwarding evaluates the assertions a frame below where they were written,
  # where a local of the same name is invisible and a global one is not
  forwarder <- function(...) chk_any(...)
  (function() { s <- 3L; forwarder(chk_string(s), chk_number(s)) })() |>
    expect_error("cannot take a forwarded")

  # a call is a call, whatever shape it has
  chk_any({chk_string(TRUE); chk_number(1)})            |> expect_equal(1)
  chk_any(if (TRUE) chk_string(s) else chk_number(s))   |> expect_equal(s)

})


test_that("the suppression protocol is confined to chk_any()", {

  # an ordinary assertion throws, and says the same thing it always did
  chk_string(1) |> expect_error("Assertion on `1` failed")

  # a failure object never reaches a caller
  chk_any(chk_string(1), chk_number(1)) |> zmisc:::is_chk_failure() |> expect_false()

  # the flag is a private token, so a binding of that name that zmisc did not
  # write does not switch aborting off
  (function() {
    .zmisc_suppress_chk_abort <- TRUE
    chk_string(1) |> expect_error("Must be of type 'string'")
  })()

  # and the failure object is known by that token rather than by its class, so
  # an object of the same class is a value like any other
  fake <- structure(list(bullets = "nope"), class = "zmisc_chk_failure")
  chk_any(chk_class(fake, "zmisc_chk_failure"), chk_number(1)) |> expect_equal(fake)

})
