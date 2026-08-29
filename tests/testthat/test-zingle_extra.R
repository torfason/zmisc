# tests/testthat/test-zingle.R

# ---------------------------------------------------------------------------
# Flag matrix helper
#
# Each degenerate input is run against all 2^4 flag combinations. `required`
# lists the flags that must ALL be TRUE for the call to succeed; every other
# flag must be irrelevant. `required = NULL` marks an input that no flag can
# rescue. This is what enforces flag independence.
# ---------------------------------------------------------------------------

flag_names <- c("empty.ok", "na.ok.partial", "na.ok.all", "nan.ok.all")

flag_grid <- expand.grid(
  stats::setNames(rep(list(c(FALSE, TRUE)), length(flag_names)), flag_names),
  KEEP.OUT.ATTRS = FALSE
)

expect_flag_matrix <- function(x, required, value = NULL) {
  for (i in seq_len(nrow(flag_grid))) {
    flags <- as.list(flag_grid[i, , drop = FALSE])
    combo <- paste0(flag_names, "=", unlist(flags), collapse = ", ")

    # all(NULL) is TRUE, so required = character(0) means "always succeeds"
    should_work <- !is.null(required) && all(unlist(flags[required]))

    if (should_work) {
      expect_identical(do.call(zingle, c(list(x), flags)), value, info = combo)
    } else {
      expect_error(do.call(zingle, c(list(x), flags)), info = combo)
    }
  }
}

test_that("flag matrix: identical values always succeed", {
  expect_flag_matrix(c("a", "a", "a"), character(0), "a")
  expect_flag_matrix("a", character(0), "a")
  expect_flag_matrix(c(1, 1), character(0), 1)
})

test_that("flag matrix: distinct values can never be rescued", {
  expect_flag_matrix(c("a", "b"), NULL)
  expect_flag_matrix(c(1, 2), NULL)
  expect_flag_matrix(c(1, 1, 2), NULL)
})

test_that("flag matrix: empty vectors need empty.ok and nothing else", {
  expect_flag_matrix(character(0), "empty.ok", NA_character_)
  expect_flag_matrix(integer(0),   "empty.ok", NA_integer_)
  expect_flag_matrix(double(0),    "empty.ok", NA_real_)
  expect_flag_matrix(logical(0),   "empty.ok", NA)
})

test_that("flag matrix: all-NA needs na.ok.all and nothing else", {
  expect_flag_matrix(c(NA, NA),                 "na.ok.all", NA)
  expect_flag_matrix(NA,                        "na.ok.all", NA)
  expect_flag_matrix(c(NA_character_, NA_character_), "na.ok.all", NA_character_)
  expect_flag_matrix(NA_integer_,               "na.ok.all", NA_integer_)
})

test_that("flag matrix: partial NA needs na.ok.partial and nothing else", {
  expect_flag_matrix(c(NA, "a"),       "na.ok.partial", "a")
  expect_flag_matrix(c("a", NA),       "na.ok.partial", "a")
  expect_flag_matrix(c(NA, 1),         "na.ok.partial", 1)
  expect_flag_matrix(c(NA, "a", "a"),  "na.ok.partial", "a")
})

test_that("flag matrix: all-NaN needs nan.ok.all and nothing else", {
  expect_flag_matrix(c(NaN, NaN), "nan.ok.all", NaN)
  expect_flag_matrix(NaN,         "nan.ok.all", NaN)
})

test_that("flag matrix: NaN mixed with NA needs both gates", {
  expect_flag_matrix(c(NaN, NA), c("na.ok.partial", "nan.ok.all"), NaN)
  expect_flag_matrix(c(NA, NaN), c("na.ok.partial", "nan.ok.all"), NaN)
})

test_that("flag matrix: NaN mixed with a real value is a distinct-value error", {
  # No flag permits this: NaN counts as a value, so this is two distinct
  # values, not a missingness problem.
  expect_flag_matrix(c(NaN, 1), NULL)
  expect_flag_matrix(c(1, NaN), NULL)
  expect_flag_matrix(c(NaN, 1, NA), NULL)
})

# ---------------------------------------------------------------------------
# Flag independence, stated explicitly for readability
# ---------------------------------------------------------------------------

test_that("na.ok.all does not permit partial missingness", {
  expect_error(zingle(c(NA, 1), na.ok.all = TRUE))
  expect_identical(zingle(c(NA, 1), na.ok.partial = TRUE), 1)
})

test_that("na.ok.partial does not permit total missingness", {
  expect_error(zingle(c(NA, NA), na.ok.partial = TRUE))
  expect_identical(zingle(c(NA, NA), na.ok.all = TRUE), NA)
})

test_that("empty.ok and the NA flags do not substitute for each other", {
  expect_error(zingle(character(0), na.ok.all = TRUE))
  expect_error(zingle(character(0), na.ok.partial = TRUE))
  expect_error(zingle(c(NA, NA), empty.ok = TRUE))
})

test_that("nan.ok.all does not permit NA, and na flags do not permit NaN", {
  expect_error(zingle(c(NA, NA), nan.ok.all = TRUE))
  expect_error(zingle(c(NaN, NaN), na.ok.all = TRUE))
  expect_error(zingle(c(NaN, NaN), na.ok.partial = TRUE))
})

# ---------------------------------------------------------------------------
# NA / NaN semantics
# ---------------------------------------------------------------------------

test_that("NaN is treated as a value, not as missingness", {
  # It survives the missing mask and is subject to the equality check
  expect_identical(zingle(c(NaN, NaN), nan.ok.all = TRUE), NaN)
  expect_false(identical(zingle(c(NaN, NaN), nan.ok.all = TRUE), NA_real_))
})

test_that("NA_real_ and NaN are distinguished", {
  expect_identical(zingle(c(NA_real_, NA_real_), na.ok.all = TRUE), NA_real_)
  expect_identical(zingle(c(NaN, NaN), nan.ok.all = TRUE), NaN)
})

test_that("results do not depend on element order", {
  expect_identical(zingle(c(NA, 1, 1), na.ok.partial = TRUE),
                   zingle(c(1, NA, 1), na.ok.partial = TRUE))
  expect_identical(zingle(c(1, 1, NA), na.ok.partial = TRUE), 1)
})

# ---------------------------------------------------------------------------
# Type preservation
# ---------------------------------------------------------------------------

test_that("the returned NA carries the type of x for empty input", {
  expect_identical(zingle(logical(0),   empty.ok = TRUE), NA)
  expect_identical(zingle(integer(0),   empty.ok = TRUE), NA_integer_)
  expect_identical(zingle(double(0),    empty.ok = TRUE), NA_real_)
  expect_identical(zingle(character(0), empty.ok = TRUE), NA_character_)
  expect_identical(zingle(complex(0),   empty.ok = TRUE), NA_complex_)
})

test_that("the returned NA carries the type of x for all-NA input", {
  expect_identical(zingle(NA_integer_,   na.ok.all = TRUE), NA_integer_)
  expect_identical(zingle(NA_real_,      na.ok.all = TRUE), NA_real_)
  expect_identical(zingle(NA_character_, na.ok.all = TRUE), NA_character_)
})

test_that("values are returned with their own type", {
  expect_identical(zingle(c(1L, 1L)), 1L)
  expect_identical(zingle(c(1.5, 1.5)), 1.5)
  expect_identical(zingle(c(TRUE, TRUE)), TRUE)
  expect_identical(zingle(c(1 + 2i, 1 + 2i)), 1 + 2i)
  expect_identical(zingle(as.raw(c(1, 1))), as.raw(1))
})

test_that("classed atomic vectors keep their class", {
  d <- as.Date("2026-01-01")
  expect_identical(zingle(c(d, d)), d)

  empty_date <- zingle(as.Date(character(0)), empty.ok = TRUE)
  expect_s3_class(empty_date, "Date")
  expect_true(is.na(empty_date))

  f <- factor(c("a", "a"), levels = c("a", "b"))
  expect_identical(zingle(f), factor("a", levels = c("a", "b")))
  expect_error(zingle(factor(c("a", "b"))))
})

# ---------------------------------------------------------------------------
# Names
# ---------------------------------------------------------------------------

test_that("names are ignored and stripped from the result", {
  expect_identical(zingle(c(a = 1, b = 1)), 1)
  expect_null(names(zingle(c(a = 1, b = 1))))
})

test_that("names do not participate in the equality check", {
  expect_identical(zingle(c(a = "x", b = "x", c = "x")), "x")
})

test_that("names are stripped from NA returns too", {
  expect_null(names(zingle(c(a = NA, b = NA), na.ok.all = TRUE)))
  expect_null(names(zingle(c(a = NA, b = 1), na.ok.partial = TRUE)))
})

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("NULL is rejected", {
  expect_error(zingle(NULL), "NULL")
  expect_error(zingle(NULL, empty.ok = TRUE), "NULL")
})

test_that("non-atomic input is rejected with a useful hint", {
  expect_error(zingle(list(1, 1)), "atomic")
  expect_error(zingle(data.frame(x = c(1, 1))), "data\\.frame")
  expect_error(zingle(as.POSIXlt("2026-01-01")), "POSIXlt")
})

test_that("POSIXct is accepted, unlike POSIXlt", {
  ts <- as.POSIXct("2026-01-01 12:00:00", tz = "UTC")
  expect_identical(zingle(c(ts, ts)), ts)
})

test_that("positional and stale arguments are caught by the dots guard", {
  # Guards against callers carrying over the old na.rm API
  expect_error(zingle(c(1, 1), TRUE))
  expect_error(zingle(c(1, 1), na.rm = TRUE))
  expect_error(zingle(c(NA, 1), na.rm = TRUE))
})

# ---------------------------------------------------------------------------
# Error messages
# ---------------------------------------------------------------------------

test_that("the distinct-value error reports the count", {
  expect_error(zingle(c(1, 2, 3)), "3 distinct values")
  expect_error(zingle(c(1, 2, 2)), "2 distinct values")
  expect_error(zingle(c("a", "b")), "2 distinct values")
})

test_that("the partial-NA error reports the proportion missing", {
  expect_error(zingle(c(NA, NA, 1)), "2 of 3")
  expect_error(zingle(c(NA, 1)), "1 of 2")
})

test_that("errors name the flag that would permit the input", {
  expect_error(zingle(character(0)), "empty\\.ok")
  expect_error(zingle(c(NA, NA)), "na\\.ok\\.all")
  expect_error(zingle(c(NA, 1)), "na\\.ok\\.partial")
  expect_error(zingle(c(NaN, NaN)), "nan\\.ok\\.all")
})

test_that("the distinct-value check runs before the NaN gate", {
  # c(NaN, 1) should report a distinct-value problem, not suggest nan.ok.all
  expect_error(zingle(c(NaN, 1)), "distinct values")
})

# ---------------------------------------------------------------------------
# Aggregation context
# ---------------------------------------------------------------------------

test_that("zingle works as a summarise() guard", {
  skip_if_not_installed("dplyr")

  d <- dplyr::tibble(
    id     = c(1, 2, 1),
    name   = c("James", "Jack", "James"),
    fouls  = c(3, 2, 4)
  )

  res <- d |>
    dplyr::group_by(id) |>
    dplyr::summarise(name = zingle(name), total_fouls = sum(fouls))

  expect_identical(res$name, c("James", "Jack"))
  expect_identical(res$total_fouls, c(7, 2))
})

test_that("zingle fails the aggregation when a group is inconsistent", {
  skip_if_not_installed("dplyr")

  d <- dplyr::tibble(
    id   = c(1, 2, 1),
    name = c("James", "Jack", "Jammes")
  )

  expect_error(
    d |> dplyr::group_by(id) |> dplyr::summarise(name = zingle(name))
  )
})

test_that("empty groups are not silently produced", {
  skip_if_not_installed("dplyr")

  d <- dplyr::tibble(
    id   = factor(c(1, 1), levels = c(1, 2)),
    name = c("James", "James")
  )

  # The empty level 2 group has no value, and must be opted into
  expect_error(
    d |> dplyr::group_by(id, .drop = FALSE) |> dplyr::summarise(name = zingle(name))
  )
  expect_no_error(
    d |> dplyr::group_by(id, .drop = FALSE) |>
      dplyr::summarise(name = zingle(name, empty.ok = TRUE))
  )
})
