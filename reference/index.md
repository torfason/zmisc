# Package index

## Safer sampling, sequencing and aggregation

Replacements for base functions that do not change their behaviour at
awkward boundary conditions.

- [`zample()`](https://torfason.github.io/zmisc/reference/zample.md) :
  Sample from a vector in a safe way
- [`zeq()`](https://torfason.github.io/zmisc/reference/zeq.md) :
  Generate sequence in a safe way
- [`zingle()`](https://torfason.github.io/zmisc/reference/zingle.md) :
  Return the single (unique) value found in a vector

## Assertions

The `chk_*()` family: checkmate type checks that raise rlang style
errors and return their input.

- [`chk_flag()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
  [`chk_string()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
  [`chk_number()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
  [`chk_inumber()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
  [`chk_dnumber()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
  [`chk_znumber()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
  [`chk_count()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
  [`chk_day()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
  [`chk_instant()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
  [`chk_scalar()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
  [`chk_logical()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
  [`chk_character()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
  [`chk_numeric()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
  [`chk_integer()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
  [`chk_double()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
  [`chk_integerish()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
  [`chk_naturalish()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
  [`chk_factor()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
  [`chk_complex()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
  [`chk_raw()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
  [`chk_date()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
  [`chk_posixct()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
  [`chk_atomic()`](https://torfason.github.io/zmisc/reference/checkmate_rlang.md)
  : Assertion functions for scalars and atomic vectors
- [`chk_environment()`](https://torfason.github.io/zmisc/reference/checkmate_rlang_other.md)
  [`chk_list()`](https://torfason.github.io/zmisc/reference/checkmate_rlang_other.md)
  [`chk_data_frame()`](https://torfason.github.io/zmisc/reference/checkmate_rlang_other.md)
  [`chk_data_table()`](https://torfason.github.io/zmisc/reference/checkmate_rlang_other.md)
  [`chk_tibble()`](https://torfason.github.io/zmisc/reference/checkmate_rlang_other.md)
  [`chk_class()`](https://torfason.github.io/zmisc/reference/checkmate_rlang_other.md)
  [`chk_true()`](https://torfason.github.io/zmisc/reference/checkmate_rlang_other.md)
  : Assertion functions for objects that are not atomic vectors
- [`chk_dots_empty()`](https://torfason.github.io/zmisc/reference/checkmate_rlang_dots.md)
  [`chk_match()`](https://torfason.github.io/zmisc/reference/checkmate_rlang_dots.md)
  : Aliases for rlang assertions

## Glue interpolation

- [`glue_vector()`](https://torfason.github.io/zmisc/reference/glue_vector.md)
  : Glue interpolation vectors in pipes

## Value look-ups

- [`lookup()`](https://torfason.github.io/zmisc/reference/lookup.md)
  [`lookuper()`](https://torfason.github.io/zmisc/reference/lookup.md) :
  Lookup values from a lookup table

## Viewing variables

- [`notate()`](https://torfason.github.io/zmisc/reference/notate.md) :
  Embed factor levels and value labels in values.

## Other utilities

- [`asciify()`](https://torfason.github.io/zmisc/reference/asciify.md) :
  Convert non-ASCII characters to their ASCII equivalents
- [`yencode()`](https://torfason.github.io/zmisc/reference/yencode.md)
  [`yencoder()`](https://torfason.github.io/zmisc/reference/yencode.md)
  [`ydecode()`](https://torfason.github.io/zmisc/reference/yencode.md)
  [`ydecoder()`](https://torfason.github.io/zmisc/reference/yencode.md)
  : Yet (another urlencode compatible) encoding scheme
