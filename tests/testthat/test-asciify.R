
# Define test cases
test_that("asciify returns the correct output", {

  # Define input and expected output using tribble()
  test_data <- tibble::tribble(
    ~input, ~expected_output,
    "Hello world!",
      "Hello world!",
    "Hæ hæ, ég heiti Gunnar",
      "Hae hae, eg heiti Gunnar",
    "Ä Ö Ü á é ï ß Š č Ž đ ș ț",
      "A O Ue a e i ss S c Z d s t",
    "This is a mix of ASCII and non-ASCII characters: ë, ü, ß, and ö",
      "This is a mix of ASCII and non-ASCII characters: e, ue, ss, and o",
    "Here's another one: á, í, ó, ú, and é",
      "Here's another one: a, i, o, u, and e",
    "Finally, let's add some Icelandic: Þetta er íslenska",
      "Finally, let's add some Icelandic: Thetta er islenska" )

  # Test asciify() on each row of test_data
  expect_equal(asciify(test_data$input), test_data$expected_output)
})


# Define test cases
test_that("asciify verify works correctly on non-handled characters", {

test_data <- tibble::tribble(
  ~input, ~expected_output,
  "This string contains the character Ʃ",
    "This string contains the character Ʃ",
  "이 문자열은 한국어를 포함합니다",
    "이 문자열은 한국어를 포함합니다" )

  # With the default argument, each of the above should result in an error
  expect_error(asciify(test_data$input[1]))
  expect_error(asciify(test_data$input[2]))

  # Test asciify() on each row of test_data
  expect_equal(asciify(test_data$input, verify = FALSE), test_data$expected_output)

})


# Define test cases
test_that("asciify correctly asserts the type of inputs", {

  # 1. Test that passing a numeric vector to the function throws an error.
  expect_error(asciify(123))

  # 2. Test that passing a logical vector to the function throws an error.
  expect_error(asciify(c(TRUE, FALSE)))

  # 3. Test that passing a list to the function throws an error.
  expect_error(asciify(list("æ", "b", "c")))

  # 4. Test that passing a missing argument to the function throws an error.
  expect_error(asciify())

  # 5. Test that passing a non-character object, such as a data frame or matrix, to the function throws an error.
  expect_error(asciify(matrix(c("æ", "b", "c"))))

  # 6. Test that passing a non-atomic character object, such as a list or vector, to the function throws an error.
  expect_error(asciify(list("æ", "b", "c")))

  # 7. Passing non-boolean to verify throws error
  expect_error(asciify(letters, verify=1))

  # 8. Passing vector longer than 1 to verify throws error
  expect_error(asciify(letters, c(TRUE, FALSE)))

})
