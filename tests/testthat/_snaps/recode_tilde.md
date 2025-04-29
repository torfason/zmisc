# `.default` is part of common type computation

    Code
      recode_tilde(1, 1 ~ 1L, .default = "x")
    Condition
      Error in `recode_tilde()`:
      ! Can't combine `..1 (right)` <integer> and `.default` <character>.

# `NULL` formula element throws meaningful error

    Code
      recode_tilde(1, 1 ~ NULL)
    Condition
      Error in `recode_tilde()`:
      ! `..1 (right)` must be a vector, not `NULL`.

---

    Code
      recode_tilde(1, NULL ~ 1)
    Condition
      Error in `recode_tilde()`:
      ! `..1 (left)` must be a vector, not `NULL`.

# throws chained errors when formula evaluation fails

    Code
      recode_tilde(1, 1 ~ 2, 3 ~ stop("oh no!"))
    Condition
      Error in `recode_tilde()`:
      ! Failed to evaluate the right-hand side of formula 2.
      Caused by error:
      ! oh no!

---

    Code
      recode_tilde(1, 1 ~ 2, stop("oh no!") ~ 4)
    Condition
      Error in `recode_tilde()`:
      ! Failed to evaluate the left-hand side of formula 2.
      Caused by error:
      ! oh no!

