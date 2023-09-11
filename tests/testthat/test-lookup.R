
#### test lookup() ####
test_that("lookup works", {

  # Extremely simple test with letters
  expect_equal(lookup(letters, c(a="A",b="B")), c("A","B",letters[3:26]))

  # Validation data
  d.pets <- data.frame(
    name  = c("cat",    "lizard",  "parrot"),
    value = c("mammal", "reptile", "bird")
  )
  x.species  = c("lizard", "cat")
  x.kingdoms = c("reptile","mammal")

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

  # house cat, because spaces should not pose a problem
  d.in <- data.frame(
    name  = c("house cat", "lizard",  "parrot"),
    value = c("mammal",    "reptile", "bird")
  ) #|> standardize_lookup_table() |> dput()


  l.want <- list("house cat" = "mammal", lizard = "reptile", parrot = "bird")
  l.have <- standardize_lookup_table(d.in)

  expect_equal(l.have, l.want)

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
  lookup(letters[1:3], c(a="A"), NULL)
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

