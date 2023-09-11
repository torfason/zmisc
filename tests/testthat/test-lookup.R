
#### test lookup() ####
test_that("lookup works", {

  # Extremely simple test with letters
  expect_equal(lookup(letters, c(a="A", b="B")), c("A", "B", letters[3:26]))

  # Validation data
  d.pets <- data.frame(
    name  = c("cat",    "lizard",  "parrot"),
    value = c("mammal", "reptile", "bird")
  )
  x.species  = c("lizard",  "cat")
  x.kingdoms = c("reptile", "mammal")

  # Standard lookup
  expect_equal( lookup(x.species,d.pets),  x.kingdoms  )

  # Order should not matter, only the names of the name/value columns.
  d.pets.rearranged = d.pets %>%
    dplyr::mutate(ga = "ga", rb="rb", age="age") %>%
    dplyr::select(ga,name,rb,value,age)
  expect_equal( lookup(x.species,d.pets.rearranged),  x.kingdoms)
})


#### test lookuper() ####
test_that("lookuper works", {

  # Extremely simple test with letters
  lookup_letters <- lookuper(c(a = "A", b = "B"))
  expect_equal(lookup_letters(letters), c("A", "B", letters[3:26]))

  # Validation data
  d.pets <- data.frame(
    name  = c("cat",    "lizard",  "parrot"),
    value = c("mammal", "reptile", "bird")
  )
  x.species <- c("lizard", "cat")
  x.kingdoms <- c("reptile", "mammal")

  # Standard lookup
  lookup_pets <- lookuper(d.pets)
  expect_equal(lookup_pets(x.species), x.kingdoms)

  # Order should not matter, only the names of the name/value columns.
  d.pets.rearranged <- d.pets %>%
    dplyr::mutate(ga = "ga", rb = "rb", age = "age") %>%
    dplyr::select(ga, name, rb, value, age)
  lookup_pets_rearranged <- lookuper(d.pets.rearranged)
  expect_equal(lookup_pets_rearranged(x.species), x.kingdoms)
})


#### test standardize_lookup_table() ####
test_that("standardize_lookup_table works", {

  l.in <- list("house cat" = "mammal", lizard = "reptile", parrot = "bird")
  d.have <- standardize_lookup_table(l.in)

  # house cat, because spaces should not pose a problem
  d.want <- data.frame(
    key   = c("house cat", "lizard",  "parrot"),
    value = c("mammal",    "reptile", "bird")
  )
  expect_equal(d.have, d.want)

})

#### test missing values####
test_that("looking up missing values works", {
  d.lookup <- data.frame(
    name  = c("house cat", "lizard",  "parrot"),
    value = c("mammal",    "reptile", "bird")
  )

  # Default is to replace with the value in x
  have.default = lookup(c("lizard", "parrot", "house cat", "tiger", "", NA), d.lookup)
  want.default = c("reptile", "bird", "mammal", "tiger", "", NA)
  expect_equal(have.default, want.default)

  # This is also what should happen if specifying NULL as default
  have.default = lookup(c("lizard", "parrot", "house cat", "tiger", "", NA), d.lookup, .default = NULL)
  want.default = c("reptile", "bird", "mammal", "tiger", "", NA)
  expect_equal(have.default, want.default)

  # Specifying NA as the default value should work as well
  have.na = lookup(c("lizard", "parrot", "house cat", "tiger", "", NA), d.lookup, .default = NA)
  want.na = c("reptile", "bird", "mammal", NA, NA, NA)
  expect_equal(have.default, want.default)

  # Specifying a placeholder, such as "" should work as well
  #
  # Initial implementation treats NA as never being found (names cannot be NA).
  # After switching to using match, it is simple to put NA in the lookup table to customize replacement
  #
  have.empty = lookup(c("lizard", "parrot", "house cat", "tiger", "", NA), d.lookup, .default = "")
  want.empty = c("reptile", "bird", "mammal", "", "", "")
  expect_equal(have.empty, want.empty)

  # The above three cases should all work with lookupers as well

  # Default is to replace with the value in x
  lookup_animals_default <- lookuper(d.lookup)
  have.lookuper.default <- lookup_animals_default(c("lizard", "parrot", "house cat", "tiger", "", NA))
  expect_equal(have.lookuper.default, want.default)

  # This is also what should happen if specifying NULL as default
  lookup_animals_null <- lookuper(d.lookup, .default = NULL)
  have.lookuper.null <- lookup_animals_null(c("lizard", "parrot", "house cat", "tiger", "", NA))
  expect_equal(have.lookuper.null, want.default)

  # Specifying a placeholder, such as "" should work as well
  lookup_animals_empty <- lookuper(d.lookup, .default = "")
  have.lookuper.empty <- lookup_animals_empty(c("lizard", "parrot", "house cat", "tiger", "", NA))
  expect_equal(have.lookuper.empty, want.empty)

  # Specifying NA as the default value should work as well
  lookup_animals_na <- lookuper(d.lookup, .default = NA)
  have.lookuper.na <- lookup_animals_na(c("lizard", "parrot", "house cat", "tiger", "", NA))
  expect_equal(have.lookuper.na, want.na)

  # .default == NULL implies x
  abc <- letters[1:3]
  lookup(letters[1:3], c(a = "A"), .default = NULL)
})


#### test non-char input ####
test_that("non-char input works", {

  x <- 1:3
  d <- data.frame(key = 2, value = 200)

  # Default
  have.default <- c(1, 200, 3)
  expect_equal(lookup(x, d), have.default)

  # Pass .default = NULL
  expect_equal(lookup(x, d, .default = NULL), have.default)

  # Default NA
  have.na <- c(NA, 200, NA)
  expect_equal(lookup(x, d, .default = NA), have.na)

  # Default scalar
  have.scalar <- c(100, 200, 100)
  expect_equal(lookup(x, d, .default = 100), have.scalar)

  # Default vector
  have.vector <- c(3, 200, 1)
  expect_equal(lookup(x, d, .default = 3:1), have.vector)


  # Raw (note that NA is not allowed for raw)
  x.raw <- as.raw(x)
  d.raw <- d
  d.raw[] <- lapply(d.raw, as.raw)
  dflt.raw <- as.raw(3:1)
  expect_equal(lookup(x.raw, d.raw), as.raw(have.default))
  expect_equal(lookup(x.raw, d.raw, .default = NULL), as.raw(have.default))
  expect_equal(lookup(x.raw, d.raw, .default = as.raw(100)), as.raw(have.scalar))
  expect_equal(lookup(x.raw, d.raw, .default = as.raw(3:1)), as.raw(have.vector))


})

#### test non-char input ####
test_that("looking up NA values works", {

  x <- c(TRUE, NA)

  # Look up NA
  expect_equal(
    lookup(x, data.frame(key = NA, value = TRUE)),
    c(TRUE, TRUE) )

  # Look up NA, but the value is actually missing
  expect_equal(
    lookup(x, data.frame(key = NA, value = TRUE), .default = FALSE),
    c(FALSE, TRUE) )

  expect_equal(
    lookup(c("cat", "dog", "horse", NA, "lion"),
           data.frame(name = c("dog", NA), value = c("SPOT", "MISSING")),
           .default = "NOT FOUND"),
    c("NOT FOUND", "SPOT", "NOT FOUND", "MISSING", "NOT FOUND"))


})

#### test name handling ####
test_that("names and attributes of x are preserved", {

  x <- c(1, 2, 3, 4, 5)
  names(x) <- c("one", "two", "three", "four", "five")
  lab <- "A vector of numbers"
  attr(x, "label") <- lab
  d <- data.frame(key = c(2, 4, 6), value = c(200L, 400L, 600L))

  have <- lookup(x, d)
  expect_equal( as.vector(have), c(1, 200, 3, 400, 5))
  expect_equal( names(have), names(x))
  expect_equal( attr(have, "label"), lab)

  have <- lookup(x, d, .default = 0)
  expect_equal( as.vector(have), c(0, 200, 0, 400, 0))
  expect_equal( names(have), names(x))
  expect_equal( attr(have, "label"), lab)

})

#### test zero-length input ####
test_that("zero-length input works", {


    d.lookup <- data.frame(
      name  = c("house cat", "lizard",  "parrot"),
      value = c("mammal",    "reptile", "bird")
    )
    d.zl <- d.lookup |> head(0)
    x    <- c("lizard", "parrot", "lion")
    x.zl <- character()

    expect_equal(lookup(x, d.lookup), c("reptile", "bird", "lion"))
    expect_equal(lookup(x, d.zl), x)

    expect_equal(lookup(x.zl, d.lookup), x.zl)
    expect_equal(lookup(x.zl, d.zl), x.zl)
})


test_that("type mismatch errors out", {

  x <- c(1, 2, 3, 4, 5)
  d <- data.frame(key = c(2, 4, 6), value = c(200L, 400L, 600L))
  dflt <- 10

  expect_equal(lookup(x, d, .default = dflt),
               c(10, 200, 10, 400, 10))

  expect_error(lookup(x, d, .default = TRUE))
  expect_error(lookup(x, c(a = 4), .default = dflt))
  expect_error(lookup(x, data.frame(key = 1, value = as.raw(3), .default = dflt)))


})

test_that("lookup() should error with factors", {
  expect_error(
    lookup(factor(letters), c("a"="alpha")),
    "lookup.* does not support factors" )

})
