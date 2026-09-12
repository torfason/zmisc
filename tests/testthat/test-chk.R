
# Hand-written tests for the chk_*() assertion family.
#
# One test_that() block per function. Each block starts with a parameterless
# call that passes, follows with a few type mismatches, and then covers each
# parameter with at least one passing and one failing setting. The reserved
# dots are covered by passing a name the function does not have.



# Scalars and atomic vectors ---------------------------------------------

test_that("chk_flag() works for all params", {

  true_nm <- structure(TRUE, names = "so true")
  true_lb <- structure(TRUE, label = "so true")

  chk_flag(TRUE)                        |> expect_true()
  chk_flag(FALSE)                       |> expect_equal(FALSE)
  chk_flag(c(TRUE, FALSE))              |> expect_error()
  chk_flag("TRUE")                      |> expect_error()

  chk_flag(NA)                          |> expect_error()
  chk_flag(NA, na.ok = TRUE)            |> expect_equal(NA)

  chk_flag(NULL)                        |> expect_error()
  chk_flag(NULL, null.ok = TRUE)        |> expect_null()

  chk_flag(true_nm)                     |> expect_equal(true_nm)
  chk_flag(true_nm, attr.ok = FALSE)    |> expect_error()
  chk_flag(true_lb)                     |> expect_error()
  chk_flag(true_lb, attr.ok = TRUE)     |> expect_equal(true_lb)
  chk_flag(true_lb, attr.ok = "label")  |> expect_equal(true_lb)

  # scalar checks take no length argument, so the dots catch it
  chk_flag(TRUE, length = 1)            |> expect_error()

})


test_that("chk_logical() works for all params", {

  true_nm <- structure(TRUE, names = "so true")
  true_lb <- structure(TRUE, label = "so true")

  chk_logical(TRUE)                          |> expect_true()
  chk_logical(NA)                            |> expect_equal(NA)
  chk_logical(NA, na.ok = FALSE)             |> expect_error()
  chk_logical(NULL)                          |> expect_error()
  chk_logical(NULL, null.ok = TRUE)          |> expect_null()
  chk_logical(true_nm)                       |> expect_equal(true_nm)
  chk_logical(true_nm, attr.ok = FALSE)      |> expect_error()
  chk_logical(true_lb)                       |> expect_error()
  chk_logical(true_lb, attr.ok = TRUE)       |> expect_equal(true_lb)
  chk_logical(true_lb, attr.ok = "label")    |> expect_equal(true_lb)
  chk_logical(FALSE)                         |> expect_equal(FALSE)
  chk_logical(FALSE, length = c(2, Inf))     |> expect_error()
  chk_logical(c(TRUE, FALSE), length = 2)    |> expect_equal(c(TRUE, FALSE))
  chk_logical(1:2)                           |> expect_error()
  chk_logical(TRUE, bogus = TRUE)            |> expect_error()

})


test_that("chk_string() works for all params", {

  str_nm <- c(first = "abc")
  str_lb <- structure("abc", label = "first")

  chk_string("abc")                        |> expect_equal("abc")
  chk_string(c("abc", "def"))              |> expect_error()
  chk_string(42)                           |> expect_error()

  chk_string(NA_character_)                |> expect_error()
  chk_string(NA_character_, na.ok = TRUE)  |> expect_equal(NA_character_)
  chk_string(NA)                           |> expect_error()
  chk_string(NA, na.ok = TRUE)             |> expect_equal(NA)

  chk_string(NULL)                         |> expect_error()
  chk_string(NULL, null.ok = TRUE)         |> expect_null()

  chk_string(str_nm)                       |> expect_equal(str_nm)
  chk_string(str_nm, attr.ok = FALSE)      |> expect_error()
  chk_string(str_lb)                       |> expect_error()
  chk_string(str_lb, attr.ok = TRUE)       |> expect_equal(str_lb)
  chk_string(str_lb, attr.ok = "label")    |> expect_equal(str_lb)

  # range constrains nchar() for the character types
  chk_string("abc", range = c(1, 5))       |> expect_equal("abc")
  chk_string("abc", range = c(4, 10))      |> expect_error()

  chk_string("abc", length = 1)            |> expect_error()

})


test_that("chk_character() works for all params", {

  chr    <- c("abc", "def")
  chr_na <- c("abc", NA)
  chr_nm <- c(first = "abc", second = "def")
  chr_lb <- structure(c("abc", "def"), label = "letters")

  chk_character(chr)                         |> expect_equal(chr)
  chk_character(1:2)                         |> expect_error()
  chk_character(factor(chr))                 |> expect_error()

  chk_character(chr_na)                      |> expect_equal(chr_na)
  chk_character(chr_na, na.ok = FALSE)       |> expect_error()

  chk_character(NULL)                        |> expect_error()
  chk_character(NULL, null.ok = TRUE)        |> expect_null()

  chk_character(chr_nm)                      |> expect_equal(chr_nm)
  chk_character(chr_nm, attr.ok = FALSE)     |> expect_error()
  chk_character(chr_lb)                      |> expect_error()
  chk_character(chr_lb, attr.ok = TRUE)      |> expect_equal(chr_lb)
  chk_character(chr_lb, attr.ok = "label")   |> expect_equal(chr_lb)

  chk_character(chr, length = 2)             |> expect_equal(chr)
  chk_character(chr, length = 3)             |> expect_error()
  chk_character(chr, length = c(1, Inf))     |> expect_equal(chr)
  chk_character(chr, length = c(3, Inf))     |> expect_error()

  chk_character(chr, range = c(1, 3))        |> expect_equal(chr)
  chk_character(chr, range = c(4, 10))       |> expect_error()

  chk_character(chr, zero.ok = TRUE)         |> expect_error()

})


test_that("chk_number() works for all params", {

  num_nm <- c(answer = 42)
  num_lb <- structure(42, label = "answer")

  chk_number(42)                          |> expect_equal(42)
  chk_number(42L)                         |> expect_equal(42L)
  chk_number(4.5)                         |> expect_equal(4.5)
  chk_number(c(1, 2))                     |> expect_error()
  chk_number("42")                        |> expect_error()

  chk_number(NA_real_)                    |> expect_error()
  chk_number(NA_real_, na.ok = TRUE)      |> expect_equal(NA_real_)
  chk_number(NA)                          |> expect_error()
  chk_number(NA, na.ok = TRUE)            |> expect_equal(NA)

  chk_number(NULL)                        |> expect_error()
  chk_number(NULL, null.ok = TRUE)        |> expect_null()

  chk_number(num_nm)                      |> expect_equal(num_nm)
  chk_number(num_nm, attr.ok = FALSE)     |> expect_error()
  chk_number(num_lb)                      |> expect_error()
  chk_number(num_lb, attr.ok = TRUE)      |> expect_equal(num_lb)
  chk_number(num_lb, attr.ok = "label")   |> expect_equal(num_lb)

  chk_number(42, range = c(0, 100))       |> expect_equal(42)
  chk_number(42, range = c(0, 10))        |> expect_error()
  chk_number(42, range = c(0, Inf))       |> expect_equal(42)

  chk_number(42, length = 1)              |> expect_error()

})


test_that("chk_numeric() works for all params", {

  num    <- c(1, 2, 3)
  num_na <- c(1, NA, 3)
  num_nm <- c(a = 1, b = 2, c = 3)
  num_lb <- structure(c(1, 2, 3), label = "counts")

  chk_numeric(num)                         |> expect_equal(num)
  chk_numeric(1:3)                         |> expect_equal(1:3)
  chk_numeric(c("1", "2"))                 |> expect_error()

  chk_numeric(num_na)                      |> expect_equal(num_na)
  chk_numeric(num_na, na.ok = FALSE)       |> expect_error()

  chk_numeric(NULL)                        |> expect_error()
  chk_numeric(NULL, null.ok = TRUE)        |> expect_null()

  chk_numeric(num_nm)                      |> expect_equal(num_nm)
  chk_numeric(num_nm, attr.ok = FALSE)     |> expect_error()
  chk_numeric(num_lb)                      |> expect_error()
  chk_numeric(num_lb, attr.ok = TRUE)      |> expect_equal(num_lb)
  chk_numeric(num_lb, attr.ok = "label")   |> expect_equal(num_lb)

  chk_numeric(num, length = 3)             |> expect_equal(num)
  chk_numeric(num, length = 2)             |> expect_error()
  chk_numeric(num, length = c(2, Inf))     |> expect_equal(num)
  chk_numeric(num, length = c(4, Inf))     |> expect_error()

  chk_numeric(num, range = c(0, 10))       |> expect_equal(num)
  chk_numeric(num, range = c(2, 10))       |> expect_error()

  chk_numeric(num, zero.ok = FALSE)        |> expect_error()

})


test_that("chk_inumber() works for all params", {

  int_nm <- structure(42L, names = "answer")
  int_lb <- structure(42L, label = "answer")

  chk_inumber(42L)                          |> expect_equal(42L)
  chk_inumber(42)                           |> expect_error()
  chk_inumber(c(1L, 2L))                    |> expect_error()
  chk_inumber("42")                         |> expect_error()

  chk_inumber(NA_integer_)                  |> expect_error()
  chk_inumber(NA_integer_, na.ok = TRUE)    |> expect_equal(NA_integer_)
  chk_inumber(NA)                           |> expect_error()
  chk_inumber(NA, na.ok = TRUE)             |> expect_equal(NA)

  chk_inumber(NULL)                         |> expect_error()
  chk_inumber(NULL, null.ok = TRUE)         |> expect_null()

  chk_inumber(int_nm)                       |> expect_equal(int_nm)
  chk_inumber(int_nm, attr.ok = FALSE)      |> expect_error()
  chk_inumber(int_lb)                       |> expect_error()
  chk_inumber(int_lb, attr.ok = TRUE)       |> expect_equal(int_lb)
  chk_inumber(int_lb, attr.ok = "label")    |> expect_equal(int_lb)

  chk_inumber(42L, range = c(0, 100))       |> expect_equal(42L)
  chk_inumber(42L, range = c(0, 10))        |> expect_error()

  chk_inumber(42L, length = 1)              |> expect_error()

})


test_that("chk_integer() works for all params", {

  int    <- 1:3
  int_na <- c(1L, NA, 3L)
  int_nm <- c(a = 1L, b = 2L, c = 3L)
  int_lb <- structure(1:3, label = "counts")

  chk_integer(int)                         |> expect_equal(int)
  chk_integer(c(1, 2, 3))                  |> expect_error()
  chk_integer(c("1", "2"))                 |> expect_error()

  chk_integer(int_na)                      |> expect_equal(int_na)
  chk_integer(int_na, na.ok = FALSE)       |> expect_error()

  chk_integer(NULL)                        |> expect_error()
  chk_integer(NULL, null.ok = TRUE)        |> expect_null()

  chk_integer(int_nm)                      |> expect_equal(int_nm)
  chk_integer(int_nm, attr.ok = FALSE)     |> expect_error()
  chk_integer(int_lb)                      |> expect_error()
  chk_integer(int_lb, attr.ok = TRUE)      |> expect_equal(int_lb)
  chk_integer(int_lb, attr.ok = "label")   |> expect_equal(int_lb)

  chk_integer(int, length = 3)             |> expect_equal(int)
  chk_integer(int, length = 2)             |> expect_error()
  chk_integer(int, length = c(2, Inf))     |> expect_equal(int)
  chk_integer(int, length = c(4, Inf))     |> expect_error()

  chk_integer(int, range = c(0, 10))       |> expect_equal(int)
  chk_integer(int, range = c(2, 10))       |> expect_error()

  chk_integer(int, zero.ok = FALSE)        |> expect_error()

})


test_that("chk_dnumber() works for all params", {

  dbl_nm <- c(half = 4.5)
  dbl_lb <- structure(4.5, label = "half")

  chk_dnumber(4.5)                         |> expect_equal(4.5)
  chk_dnumber(4L)                          |> expect_error()
  chk_dnumber(c(4.5, 5.5))                 |> expect_error()
  chk_dnumber("4.5")                       |> expect_error()

  chk_dnumber(NA_real_)                    |> expect_error()
  chk_dnumber(NA_real_, na.ok = TRUE)      |> expect_equal(NA_real_)

  chk_dnumber(NULL)                        |> expect_error()
  chk_dnumber(NULL, null.ok = TRUE)        |> expect_null()

  chk_dnumber(dbl_nm)                      |> expect_equal(dbl_nm)
  chk_dnumber(dbl_nm, attr.ok = FALSE)     |> expect_error()
  chk_dnumber(dbl_lb)                      |> expect_error()
  chk_dnumber(dbl_lb, attr.ok = TRUE)      |> expect_equal(dbl_lb)
  chk_dnumber(dbl_lb, attr.ok = "label")   |> expect_equal(dbl_lb)

  chk_dnumber(4.5, range = c(0, 10))       |> expect_equal(4.5)
  chk_dnumber(4.5, range = c(5, 10))       |> expect_error()

  chk_dnumber(4.5, length = 1)             |> expect_error()

})


test_that("chk_double() works for all params", {

  dbl    <- c(1.5, 2.5)
  dbl_na <- c(1.5, NA)
  dbl_nm <- c(a = 1.5, b = 2.5)
  dbl_lb <- structure(c(1.5, 2.5), label = "halves")

  chk_double(dbl)                         |> expect_equal(dbl)
  chk_double(2L)                          |> expect_error()
  chk_double(1:2)                         |> expect_error()
  chk_double(TRUE)                        |> expect_error()
  chk_double(c("1.5", "2.5"))             |> expect_error()

  chk_double(dbl_na)                      |> expect_equal(dbl_na)
  chk_double(dbl_na, na.ok = FALSE)       |> expect_error()

  chk_double(NULL)                        |> expect_error()
  chk_double(NULL, null.ok = TRUE)        |> expect_null()

  chk_double(dbl_nm)                      |> expect_equal(dbl_nm)
  chk_double(dbl_nm, attr.ok = FALSE)     |> expect_error()
  chk_double(dbl_lb)                      |> expect_error()
  chk_double(dbl_lb, attr.ok = TRUE)      |> expect_equal(dbl_lb)
  chk_double(dbl_lb, attr.ok = "label")   |> expect_equal(dbl_lb)

  chk_double(dbl, length = 2)             |> expect_equal(dbl)
  chk_double(dbl, length = 3)             |> expect_error()
  chk_double(dbl, length = c(1, Inf))     |> expect_equal(dbl)
  chk_double(dbl, length = c(3, Inf))     |> expect_error()

  chk_double(dbl, range = c(0, 10))       |> expect_equal(dbl)
  chk_double(dbl, range = c(2, 10))       |> expect_error()

  chk_double(dbl, zero.ok = FALSE)        |> expect_error()

})


test_that("chk_znumber() works for all params", {

  znm_nm <- c(below = -4)
  znm_lb <- structure(-4, label = "below")

  chk_znumber(4)                          |> expect_equal(4)
  chk_znumber(4L)                         |> expect_equal(4L)
  chk_znumber(-4)                         |> expect_equal(-4)
  chk_znumber(4.5)                        |> expect_error()
  chk_znumber(c(1, 2))                    |> expect_error()

  chk_znumber(NA_real_)                   |> expect_error()
  chk_znumber(NA_real_, na.ok = TRUE)     |> expect_equal(NA_real_)
  chk_znumber(NA)                         |> expect_error()
  chk_znumber(NA, na.ok = TRUE)           |> expect_equal(NA)

  chk_znumber(NULL)                       |> expect_error()
  chk_znumber(NULL, null.ok = TRUE)       |> expect_null()

  chk_znumber(znm_nm)                     |> expect_equal(znm_nm)
  chk_znumber(znm_nm, attr.ok = FALSE)    |> expect_error()
  chk_znumber(znm_lb)                     |> expect_error()
  chk_znumber(znm_lb, attr.ok = TRUE)     |> expect_equal(znm_lb)
  chk_znumber(znm_lb, attr.ok = "label")  |> expect_equal(znm_lb)

  chk_znumber(4, range = c(-10, 10))      |> expect_equal(4)
  chk_znumber(4, range = c(-10, 2))       |> expect_error()

  chk_znumber(4, zero.ok = FALSE)         |> expect_error()

})


test_that("chk_integerish() works for all params", {

  zvc    <- c(1, 2, 3)
  zvc_na <- c(1, NA, 3)
  zvc_nm <- c(a = 1, b = 2, c = 3)
  zvc_lb <- structure(c(1, 2, 3), label = "counts")

  chk_integerish(zvc)                         |> expect_equal(zvc)
  chk_integerish(1:3)                         |> expect_equal(1:3)
  chk_integerish(c(1, 2.5))                   |> expect_error()
  chk_integerish(c("1", "2"))                 |> expect_error()

  chk_integerish(zvc_na)                      |> expect_equal(zvc_na)
  chk_integerish(zvc_na, na.ok = FALSE)       |> expect_error()

  chk_integerish(NULL)                        |> expect_error()
  chk_integerish(NULL, null.ok = TRUE)        |> expect_null()

  chk_integerish(zvc_nm)                      |> expect_equal(zvc_nm)
  chk_integerish(zvc_nm, attr.ok = FALSE)     |> expect_error()
  chk_integerish(zvc_lb)                      |> expect_error()
  chk_integerish(zvc_lb, attr.ok = TRUE)      |> expect_equal(zvc_lb)
  chk_integerish(zvc_lb, attr.ok = "label")   |> expect_equal(zvc_lb)

  chk_integerish(zvc, length = 3)             |> expect_equal(zvc)
  chk_integerish(zvc, length = 2)             |> expect_error()
  chk_integerish(zvc, length = c(2, Inf))     |> expect_equal(zvc)
  chk_integerish(zvc, length = c(4, Inf))     |> expect_error()

  chk_integerish(zvc, range = c(-10, 10))     |> expect_equal(zvc)
  chk_integerish(zvc, range = c(2, 10))       |> expect_error()

  chk_integerish(zvc, zero.ok = FALSE)        |> expect_error()

})


test_that("chk_count() works for all params", {

  cnt_nm <- structure(3, names = "three")
  cnt_lb <- structure(3, label = "three")

  chk_count(3)                          |> expect_equal(3)
  chk_count(3L)                         |> expect_equal(3L)
  chk_count(-1)                         |> expect_error()
  chk_count(2.5)                        |> expect_error()
  chk_count(c(1, 2))                    |> expect_error()

  chk_count(NA_integer_)                |> expect_error()
  chk_count(NA_integer_, na.ok = TRUE)  |> expect_equal(NA_integer_)

  chk_count(0)                          |> expect_equal(0)
  chk_count(0, zero.ok = FALSE)         |> expect_error()
  chk_count(3, zero.ok = FALSE)         |> expect_equal(3)

  chk_count(NULL)                       |> expect_error()
  chk_count(NULL, null.ok = TRUE)       |> expect_null()

  chk_count(cnt_nm)                     |> expect_equal(cnt_nm)
  chk_count(cnt_nm, attr.ok = FALSE)    |> expect_error()
  chk_count(cnt_lb)                     |> expect_error()
  chk_count(cnt_lb, attr.ok = TRUE)     |> expect_equal(cnt_lb)
  chk_count(cnt_lb, attr.ok = "label")  |> expect_equal(cnt_lb)

  chk_count(3, range = c(0, 10))        |> expect_error()

})


test_that("chk_naturalish() works for all params", {

  nat    <- c(0, 1, 2)
  nat_na <- c(0, NA, 2)
  nat_nm <- c(a = 0, b = 1, c = 2)
  nat_lb <- structure(c(0, 1, 2), label = "counts")

  chk_naturalish(nat)                         |> expect_equal(nat)
  chk_naturalish(0:2)                         |> expect_equal(0:2)
  chk_naturalish(c(-1, 1))                    |> expect_error()
  chk_naturalish(c(1, 2.5))                   |> expect_error()

  chk_naturalish(nat_na)                      |> expect_equal(nat_na)
  chk_naturalish(nat_na, na.ok = FALSE)       |> expect_error()

  chk_naturalish(nat, zero.ok = FALSE)        |> expect_error()
  chk_naturalish(c(1, 2), zero.ok = FALSE)    |> expect_equal(c(1, 2))

  chk_naturalish(NULL)                        |> expect_error()
  chk_naturalish(NULL, null.ok = TRUE)        |> expect_null()

  chk_naturalish(nat_nm)                      |> expect_equal(nat_nm)
  chk_naturalish(nat_nm, attr.ok = FALSE)     |> expect_error()
  chk_naturalish(nat_lb)                      |> expect_error()
  chk_naturalish(nat_lb, attr.ok = TRUE)      |> expect_equal(nat_lb)
  chk_naturalish(nat_lb, attr.ok = "label")   |> expect_equal(nat_lb)

  chk_naturalish(nat, length = 3)             |> expect_equal(nat)
  chk_naturalish(nat, length = 2)             |> expect_error()
  chk_naturalish(nat, length = c(2, Inf))     |> expect_equal(nat)
  chk_naturalish(nat, length = c(4, Inf))     |> expect_error()

  chk_naturalish(nat, range = c(0, 10))       |> expect_equal(nat)
  chk_naturalish(nat, range = c(1, 10))       |> expect_error()

  chk_naturalish(nat, bogus = TRUE)           |> expect_error()

})


test_that("chk_factor() works for all params", {

  fct    <- factor(c("a", "b"))
  fct_na <- factor(c("a", NA))
  fct_nm <- structure(factor(c("a", "b")), names = c("x", "y"))
  fct_lb <- structure(factor(c("a", "b")), label = "letters")

  chk_factor(fct)                         |> expect_equal(fct)
  chk_factor(c("a", "b"))                 |> expect_error()
  chk_factor(1:2)                         |> expect_error()

  chk_factor(fct_na)                      |> expect_equal(fct_na)
  chk_factor(fct_na, na.ok = FALSE)       |> expect_error()

  chk_factor(NULL)                        |> expect_error()
  chk_factor(NULL, null.ok = TRUE)        |> expect_null()

  # levels and class are intrinsic to the type, so attr.ok does not see them
  chk_factor(fct, attr.ok = FALSE)        |> expect_equal(fct)
  chk_factor(fct_nm)                      |> expect_equal(fct_nm)
  chk_factor(fct_nm, attr.ok = FALSE)     |> expect_error()
  chk_factor(fct_lb)                      |> expect_error()
  chk_factor(fct_lb, attr.ok = TRUE)      |> expect_equal(fct_lb)
  chk_factor(fct_lb, attr.ok = "label")   |> expect_equal(fct_lb)

  chk_factor(fct, length = 2)             |> expect_equal(fct)
  chk_factor(fct, length = 3)             |> expect_error()
  chk_factor(fct, length = c(1, Inf))     |> expect_equal(fct)
  chk_factor(fct, length = c(3, Inf))     |> expect_error()

  chk_factor(fct, range = c(1, 2))        |> expect_error()

})


test_that("chk_complex() works for all params", {

  cpx    <- c(1 + 2i, 3 + 4i)
  cpx_na <- c(1 + 2i, NA_complex_)
  cpx_nm <- c(a = 1 + 2i, b = 3 + 4i)
  cpx_lb <- structure(c(1 + 2i, 3 + 4i), label = "phases")

  chk_complex(cpx)                         |> expect_equal(cpx)
  chk_complex(c(1, 2))                     |> expect_error()
  chk_complex(c("1+2i"))                   |> expect_error()

  chk_complex(cpx_na)                      |> expect_equal(cpx_na)
  chk_complex(cpx_na, na.ok = FALSE)       |> expect_error()

  chk_complex(NULL)                        |> expect_error()
  chk_complex(NULL, null.ok = TRUE)        |> expect_null()

  chk_complex(cpx_nm)                      |> expect_equal(cpx_nm)
  chk_complex(cpx_nm, attr.ok = FALSE)     |> expect_error()
  chk_complex(cpx_lb)                      |> expect_error()
  chk_complex(cpx_lb, attr.ok = TRUE)      |> expect_equal(cpx_lb)
  chk_complex(cpx_lb, attr.ok = "label")   |> expect_equal(cpx_lb)

  chk_complex(cpx, length = 2)             |> expect_equal(cpx)
  chk_complex(cpx, length = 3)             |> expect_error()
  chk_complex(cpx, length = c(1, Inf))     |> expect_equal(cpx)
  chk_complex(cpx, length = c(3, Inf))     |> expect_error()

  chk_complex(cpx, range = c(0, 10))       |> expect_error()

})


test_that("chk_raw() works for all params", {

  raw_vec <- as.raw(c(1, 255))
  raw_nm  <- structure(as.raw(c(1, 255)), names = c("a", "b"))
  raw_lb  <- structure(as.raw(c(1, 255)), label = "bytes")

  chk_raw(raw_vec)                      |> expect_equal(raw_vec)
  chk_raw(c(1, 255))                    |> expect_error()
  chk_raw("01")                         |> expect_error()

  chk_raw(NULL)                         |> expect_error()
  chk_raw(NULL, null.ok = TRUE)         |> expect_null()

  chk_raw(raw_vec, attr.ok = FALSE)     |> expect_equal(raw_vec)
  chk_raw(raw_nm)                       |> expect_equal(raw_nm)
  chk_raw(raw_nm, attr.ok = FALSE)      |> expect_error()
  chk_raw(raw_lb)                       |> expect_error()
  chk_raw(raw_lb, attr.ok = TRUE)       |> expect_equal(raw_lb)
  chk_raw(raw_lb, attr.ok = "label")    |> expect_equal(raw_lb)

  chk_raw(raw_vec, length = 2)          |> expect_equal(raw_vec)
  chk_raw(raw_vec, length = 3)          |> expect_error()
  chk_raw(raw_vec, length = c(1, Inf))  |> expect_equal(raw_vec)
  chk_raw(raw_vec, length = c(3, Inf))  |> expect_error()

  # raw has no missing value, and therefore no na.ok
  chk_raw(raw_vec, na.ok = TRUE)        |> expect_error()

})


test_that("chk_day() works for all params", {

  day    <- as.Date("2024-06-01")
  day_na <- as.Date(NA_character_)
  day_nm <- structure(as.Date("2024-06-01"), names = "start")
  day_lb <- structure(as.Date("2024-06-01"), label = "start")

  day_range_in  <- as.Date(c("2024-01-01", "2024-12-31"))
  day_range_out <- as.Date(c("2025-01-01", "2025-12-31"))

  chk_day(day)                             |> expect_equal(day)
  chk_day(as.Date(c("2024-06-01",
                    "2024-06-02")))        |> expect_error()
  chk_day("2024-06-01")                    |> expect_error()
  chk_day(19875)                           |> expect_error()

  chk_day(day_na)                          |> expect_error()
  chk_day(day_na, na.ok = TRUE)            |> expect_equal(day_na)

  chk_day(NULL)                            |> expect_error()
  chk_day(NULL, null.ok = TRUE)            |> expect_null()

  chk_day(day, attr.ok = FALSE)            |> expect_equal(day)
  chk_day(day_nm)                          |> expect_equal(day_nm)
  chk_day(day_nm, attr.ok = FALSE)         |> expect_error()
  chk_day(day_lb)                          |> expect_error()
  chk_day(day_lb, attr.ok = TRUE)          |> expect_equal(day_lb)
  chk_day(day_lb, attr.ok = "label")       |> expect_equal(day_lb)

  chk_day(day, range = day_range_in)       |> expect_equal(day)
  chk_day(day, range = day_range_out)      |> expect_error()

  chk_day(day, length = 1)                 |> expect_error()

})


test_that("chk_date() works for all params", {

  dte    <- as.Date(c("2024-06-01", "2024-06-02"))
  dte_na <- as.Date(c("2024-06-01", NA))
  dte_nm <- structure(as.Date(c("2024-06-01", "2024-06-02")),
                      names = c("from", "to"))
  dte_lb <- structure(as.Date(c("2024-06-01", "2024-06-02")),
                      label = "window")

  dte_range_in  <- as.Date(c("2024-01-01", "2024-12-31"))
  dte_range_out <- as.Date(c("2025-01-01", "2025-12-31"))

  chk_date(dte)                            |> expect_equal(dte)
  chk_date(c("2024-06-01", "2024-06-02"))  |> expect_error()
  chk_date(c(19875, 19876))                |> expect_error()

  chk_date(dte_na)                         |> expect_equal(dte_na)
  chk_date(dte_na, na.ok = FALSE)          |> expect_error()

  chk_date(NULL)                           |> expect_error()
  chk_date(NULL, null.ok = TRUE)           |> expect_null()

  chk_date(dte, attr.ok = FALSE)           |> expect_equal(dte)
  chk_date(dte_nm)                         |> expect_equal(dte_nm)
  chk_date(dte_nm, attr.ok = FALSE)        |> expect_error()
  chk_date(dte_lb)                         |> expect_error()
  chk_date(dte_lb, attr.ok = TRUE)         |> expect_equal(dte_lb)
  chk_date(dte_lb, attr.ok = "label")      |> expect_equal(dte_lb)

  chk_date(dte, length = 2)                |> expect_equal(dte)
  chk_date(dte, length = 3)                |> expect_error()
  chk_date(dte, length = c(1, Inf))        |> expect_equal(dte)
  chk_date(dte, length = c(3, Inf))        |> expect_error()

  chk_date(dte, range = dte_range_in)      |> expect_equal(dte)
  chk_date(dte, range = dte_range_out)     |> expect_error()

  chk_date(dte, zero.ok = FALSE)           |> expect_error()

})


test_that("chk_instant() works for all params", {

  inst    <- as.POSIXct("2024-06-01 12:00:00", tz = "UTC")
  inst_na <- as.POSIXct(NA_character_, tz = "UTC")
  inst_nm <- structure(as.POSIXct("2024-06-01 12:00:00", tz = "UTC"),
                       names = "start")
  inst_lb <- structure(as.POSIXct("2024-06-01 12:00:00", tz = "UTC"),
                       label = "start")

  inst_range_in  <- as.POSIXct(c("2024-01-01 00:00:00",
                                 "2024-12-31 23:59:59"), tz = "UTC")
  inst_range_out <- as.POSIXct(c("2025-01-01 00:00:00",
                                 "2025-12-31 23:59:59"), tz = "UTC")

  chk_instant(inst)                          |> expect_equal(inst)
  chk_instant(c(inst, inst))                 |> expect_error()
  chk_instant("2024-06-01 12:00:00")         |> expect_error()
  chk_instant(as.Date("2024-06-01"))         |> expect_error()

  chk_instant(inst_na)                       |> expect_error()
  chk_instant(inst_na, na.ok = TRUE)         |> expect_equal(inst_na)

  chk_instant(NULL)                          |> expect_error()
  chk_instant(NULL, null.ok = TRUE)          |> expect_null()

  # tzone is intrinsic to the type, so attr.ok = FALSE is only tested to fail
  chk_instant(inst_nm)                       |> expect_equal(inst_nm)
  chk_instant(inst_nm, attr.ok = FALSE)      |> expect_error()
  chk_instant(inst_lb)                       |> expect_error()
  chk_instant(inst_lb, attr.ok = TRUE)       |> expect_equal(inst_lb)
  chk_instant(inst_lb, attr.ok = "label")    |> expect_equal(inst_lb)

  chk_instant(inst, range = inst_range_in)   |> expect_equal(inst)
  chk_instant(inst, range = inst_range_out)  |> expect_error()

  chk_instant(inst, length = 1)              |> expect_error()

})


test_that("chk_posixct() works for all params", {

  pct    <- as.POSIXct(c("2024-06-01 12:00:00",
                         "2024-06-02 12:00:00"), tz = "UTC")
  pct_na <- as.POSIXct(c("2024-06-01 12:00:00", NA), tz = "UTC")
  pct_nm <- structure(pct, names = c("from", "to"))
  pct_lb <- structure(pct, label = "window")

  pct_range_in  <- as.POSIXct(c("2024-01-01 00:00:00",
                                "2024-12-31 23:59:59"), tz = "UTC")
  pct_range_out <- as.POSIXct(c("2025-01-01 00:00:00",
                                "2025-12-31 23:59:59"), tz = "UTC")

  chk_posixct(pct)                          |> expect_equal(pct)
  chk_posixct(as.Date(c("2024-06-01")))     |> expect_error()
  chk_posixct(c("2024-06-01 12:00:00"))     |> expect_error()

  chk_posixct(pct_na)                       |> expect_equal(pct_na)
  chk_posixct(pct_na, na.ok = FALSE)        |> expect_error()

  chk_posixct(NULL)                         |> expect_error()
  chk_posixct(NULL, null.ok = TRUE)         |> expect_null()

  chk_posixct(pct_nm)                       |> expect_equal(pct_nm)
  chk_posixct(pct_nm, attr.ok = FALSE)      |> expect_error()
  chk_posixct(pct_lb)                       |> expect_error()
  chk_posixct(pct_lb, attr.ok = TRUE)       |> expect_equal(pct_lb)
  chk_posixct(pct_lb, attr.ok = "label")    |> expect_equal(pct_lb)

  chk_posixct(pct, length = 2)              |> expect_equal(pct)
  chk_posixct(pct, length = 3)              |> expect_error()
  chk_posixct(pct, length = c(1, Inf))      |> expect_equal(pct)
  chk_posixct(pct, length = c(3, Inf))      |> expect_error()

  chk_posixct(pct, range = pct_range_in)    |> expect_equal(pct)
  chk_posixct(pct, range = pct_range_out)   |> expect_error()

  chk_posixct(pct, zero.ok = FALSE)         |> expect_error()

})


test_that("chk_scalar() works for all params", {

  sca_nm <- structure("a", names = "first")
  sca_lb <- structure("a", label = "first")

  chk_scalar("a")                         |> expect_equal("a")
  chk_scalar(42)                          |> expect_equal(42)
  chk_scalar(TRUE)                        |> expect_true()
  chk_scalar(1:3)                         |> expect_error()
  chk_scalar(list(1))                     |> expect_error()

  chk_scalar(NA)                          |> expect_error()
  chk_scalar(NA, na.ok = TRUE)            |> expect_equal(NA)

  chk_scalar(NULL)                        |> expect_error()
  chk_scalar(NULL, null.ok = TRUE)        |> expect_null()

  chk_scalar(sca_nm)                      |> expect_equal(sca_nm)
  chk_scalar(sca_nm, attr.ok = FALSE)     |> expect_error()
  chk_scalar(sca_lb)                      |> expect_error()
  chk_scalar(sca_lb, attr.ok = TRUE)      |> expect_equal(sca_lb)
  chk_scalar(sca_lb, attr.ok = "label")   |> expect_equal(sca_lb)

  chk_scalar("a", length = 1)             |> expect_error()

})


test_that("chk_atomic() works for all params", {

  atm    <- c(1, 2, 3)
  atm_na <- c(1, NA, 3)
  atm_nm <- c(a = 1, b = 2, c = 3)
  atm_lb <- structure(c(1, 2, 3), label = "counts")

  chk_atomic(atm)                         |> expect_equal(atm)
  chk_atomic(c("a", "b"))                 |> expect_equal(c("a", "b"))
  chk_atomic(as.raw(1))                   |> expect_equal(as.raw(1))
  chk_atomic(list(1, 2))                  |> expect_error()
  chk_atomic(data.frame(a = 1))           |> expect_error()

  chk_atomic(atm_na)                      |> expect_equal(atm_na)
  chk_atomic(atm_na, na.ok = FALSE)       |> expect_error()

  chk_atomic(atm, attr.ok = FALSE)        |> expect_equal(atm)
  chk_atomic(atm_nm)                      |> expect_equal(atm_nm)
  chk_atomic(atm_nm, attr.ok = FALSE)     |> expect_error()
  chk_atomic(atm_lb)                      |> expect_error()
  chk_atomic(atm_lb, attr.ok = TRUE)      |> expect_equal(atm_lb)
  chk_atomic(atm_lb, attr.ok = "label")   |> expect_equal(atm_lb)

  chk_atomic(atm, length = 3)             |> expect_equal(atm)
  chk_atomic(atm, length = 2)             |> expect_error()
  chk_atomic(atm, length = c(2, Inf))     |> expect_equal(atm)
  chk_atomic(atm, length = c(4, Inf))     |> expect_error()

  # chk_atomic() takes no null.ok
  chk_atomic(atm, null.ok = TRUE)         |> expect_error()

})


# Containers, classes, and conditions -------------------------------------

test_that("chk_environment() works for all params", {

  env <- new.env()
  env$alpha <- 1

  chk_environment(env)                       |> expect_equal(env)
  chk_environment(list(alpha = 1))           |> expect_error()
  chk_environment(1:3)                       |> expect_error()

  chk_environment(NULL)                      |> expect_error()
  chk_environment(NULL, null.ok = TRUE)      |> expect_null()

  chk_environment(env, contains = "alpha")   |> expect_equal(env)
  chk_environment(env, contains = "beta")    |> expect_error()

  chk_environment(env, attr.ok = FALSE)      |> expect_error()

})


test_that("chk_list() works for all params", {

  lst <- list(1, "a")

  chk_list(lst)                        |> expect_equal(lst)
  chk_list(list())                     |> expect_equal(list())
  chk_list(1:3)                        |> expect_error()
  chk_list(data.frame(a = 1))          |> expect_error()

  chk_list(NULL)                       |> expect_error()
  chk_list(NULL, null.ok = TRUE)       |> expect_null()

  chk_list(lst, length = 2)            |> expect_equal(lst)
  chk_list(lst, length = 3)            |> expect_error()
  chk_list(lst, length = c(1, Inf))    |> expect_equal(lst)
  chk_list(lst, length = c(3, Inf))    |> expect_error()

  # containers take no attr.ok
  chk_list(lst, attr.ok = FALSE)       |> expect_error()

})


test_that("chk_data_frame() works for all params", {

  df <- data.frame(a = 1:2, b = c("x", "y"))

  chk_data_frame(df)                     |> expect_equal(df)
  chk_data_frame(list(a = 1:2))          |> expect_error()
  chk_data_frame(1:3)                    |> expect_error()

  chk_data_frame(NULL)                   |> expect_error()
  chk_data_frame(NULL, null.ok = TRUE)   |> expect_null()

  chk_data_frame(df, length = 2)         |> expect_error()

})


test_that("chk_data_table() works for all params", {

  skip_if_not_installed("data.table")

  dt <- data.table::data.table(a = 1:2, b = c("x", "y"))

  chk_data_table(dt)                     |> expect_equal(dt)
  chk_data_table(data.frame(a = 1:2))    |> expect_error()
  chk_data_table(list(a = 1:2))          |> expect_error()

  chk_data_table(NULL)                   |> expect_error()
  chk_data_table(NULL, null.ok = TRUE)   |> expect_null()

  chk_data_table(dt, length = 2)         |> expect_error()

})


test_that("chk_tibble() works for all params", {

  skip_if_not_installed("tibble")

  tbl <- tibble::tibble(a = 1:2, b = c("x", "y"))

  chk_tibble(tbl)                     |> expect_equal(tbl)
  chk_tibble(data.frame(a = 1:2))     |> expect_error()
  chk_tibble(list(a = 1:2))           |> expect_error()

  chk_tibble(NULL)                    |> expect_error()
  chk_tibble(NULL, null.ok = TRUE)    |> expect_null()

  chk_tibble(tbl, length = 2)         |> expect_error()

})


test_that("chk_class() works for all params", {

  obj <- structure(1, class = c("alpha", "beta"))

  chk_class(obj, "alpha")                            |> expect_equal(obj)
  chk_class(obj, "beta")                             |> expect_equal(obj)
  chk_class(obj, c("alpha", "beta"))                 |> expect_equal(obj)
  chk_class(obj, "gamma")                            |> expect_error()
  chk_class(obj, c("alpha", "gamma"))                |> expect_error()
  chk_class(1, "alpha")                              |> expect_error()

  chk_class(NULL, "alpha")                           |> expect_error()
  chk_class(NULL, "alpha", null.ok = TRUE)           |> expect_null()

  chk_class(obj, c("alpha", "beta"), ordered = TRUE) |> expect_equal(obj)
  chk_class(obj, c("beta", "alpha"), ordered = TRUE) |> expect_error()
  chk_class(obj, c("beta", "alpha"))                 |> expect_equal(obj)

  chk_class(obj, "alpha", na.ok = TRUE)              |> expect_error()

})


test_that("chk_true() works for all params", {

  myvar <- 4
  myna  <- NA

  chk_true(myvar == 4)                   |> expect_true()
  chk_true(myvar == 5)                   |> expect_error()
  chk_true(myvar == myna)                |> expect_error()
  chk_true(myvar == myna, na.ok = TRUE)  |> expect_equal(NA)
  chk_true(c(TRUE, TRUE))                |> expect_error()
  chk_true(1)                            |> expect_error()
  chk_true(NULL)                         |> expect_error()
  chk_true(TRUE, null.ok = TRUE)         |> expect_error()

})


# Aliases for rlang assertions --------------------------------------------

test_that("chk_dots_empty() works for all params", {

  f_dots <- function(...) { chk_dots_empty(); "ok" }
  f_x_dots <- function(x, ...) { chk_dots_empty(); x }

  f_dots()             |> expect_equal("ok")
  f_dots(1)            |> expect_error()
  f_dots(bogus = 1)    |> expect_error()

  f_x_dots(1)          |> expect_equal(1)
  f_x_dots(1, 2)       |> expect_error()

  # Using ...length() as a pre-gate has same behavior and is faster
  f_len_dots <- function(x, ...) { if (...length()) chk_dots_empty(); x}
  f_len_dots(1)        |> expect_equal(1)
  f_len_dots(1, 2)     |> expect_error()

})


test_that("chk_match() works for all params", {

  f_match <- function(type = c("alpha", "beta")) chk_match(type)
  alpha <- "alpha"
  beta  <- "beta"
  gamma <- "gamma"
  ab <- c("alpha", "beta")
  empty <- character()

  # NOTE: Literals are not accepted by chk_match() / arg_match()
  chk_match("alpha", c("alpha", "beta"))        |> expect_error()

  f_match()                                     |> expect_equal("alpha")
  f_match("beta")                               |> expect_equal("beta")
  f_match("gamma")                              |> expect_error()

  chk_match(alpha, c("alpha", "beta"))          |> expect_equal("alpha")
  chk_match(beta,  c("alpha", "beta"))          |> expect_equal("beta")
  chk_match(gamma, c("alpha", "beta"))          |> expect_error()

  # NOTE: multiple elements in arg do NOT automatically cause an error,
  #       if arg has exactly the same elements as values, the first
  #       element in arg is returned
  f_match(c("alpha", "beta"))                   |> expect_equal("alpha")
  f_match(c("beta", "alpha"))                   |> expect_equal("beta")
  f_match(c("alpha", "gamma"))                  |> expect_error()
  f_match(character())                          |> expect_error()
  chk_match(ab, c("alpha", "beta"))             |> expect_equal("alpha")
  chk_match(ab, c("alpha", "gamma"))            |> expect_error()
  chk_match(ab, c("beta", "gamma"))             |> expect_error()

  # multiple = TRUE specifies a more reasonable handling of multi arg
  chk_match(ab, c("alpha", "beta", "gamma"), multiple = TRUE) |> expect_equal(c("alpha", "beta"))
  chk_match(ab, c("beta", "alpha"), multiple = TRUE)          |> expect_equal(c("alpha", "beta"))
  chk_match(ab, c("alpha", "gamma"), multiple = TRUE)         |> expect_error()
  chk_match(empty, c("alpha", "beta"), multiple = TRUE)       |> expect_equal(character())

  # error_arg: the name the failure is reported against
  chk_match(gamma, c("alpha", "beta"), error_arg = "flavour") |> expect_error("flavour")
  chk_match(alpha, c("alpha", "beta"), error_arg = "flavour") |> expect_equal("alpha")

  # Leaking into ... is an error
  chk_match("alpha", c("alpha", "beta"), bogus = TRUE) |> expect_error()

})
