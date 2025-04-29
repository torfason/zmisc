#' Recode values using tilde syntax
#'
#' @description
#' Recode elements of a vector using a series of formulas (`lhs ~ rhs`) passed
#' via `...`. Each `lhs` is matched against elements of `x`, and the
#' corresponding `rhs` provides the new value.
#'
#' This function is closely based on [dplyr::case_match()] with minimal changes
#' to make it more intuitive for re-coding tasks. In particular, rather than
#' setting unmatched values to `NA` by default, they remain unchanged
#' `.default`, which itself defaults to `x`. The output type can be controlled
#' with `.ptype`. `.ptype` defaults to `.default`, which means that type can be
#' changed by setting `.default` to either `NA` or to a value of the same type
#' as the `rhs` formula values. Incompatibility between the `rhs` values and the
#' `.ptype` results in a type error.
#'
#' @param x A vector to recode.
#' @param ... Formulae specifying recoding rules, recoding from `lhs` to `rhs`.
#' @param .default Default value for unmatched inputs. Defaults to `x`.
#' @param .ptype Optional output type, defaults to `.default`. All values in
#'   `rhs` and `.default` must be mutually compatible with `.ptype`
#'
#' @return A vector with recoded values.
#'
#' @examples
#' recode_tilde(letters, "a" ~ "first", "z" ~ "last")
#' recode_tilde(1:5, 1 ~ 10, 2 ~ 20)
#' # Recoding to different type requires explicit .default values
#' recode_tilde(1:4, 1 ~ "low", 2 ~ "medium", 3 ~ "high", .default = NA)
#'
#' @export
recode_tilde <- function(x, ..., .default = x, .ptype = NULL) {
  args <- rlang::list2(...)

  args <- dplyr_case_formula_evaluate(
    args = args,
    default_env = rlang::caller_env(),
    dots_env = rlang::current_env(),
    error_call = rlang::current_env()
  )

  haystacks <- args$lhs
  values <- args$rhs

  dplyr_vec_case_match(
    needles = x,
    haystacks = haystacks,
    values = values,
    needles_arg = "x",
    haystacks_arg = "",
    values_arg = "",
    default = .default,
    default_arg = ".default",
    ptype = .ptype,
    call = rlang::current_env()
  )
}


# Adapt dplyr:::case_formula_evaluate() for dependence on rlang+vctrs+glue only
dplyr_case_formula_evaluate <- function(args, default_env, dots_env, error_call) {

  # Compact function
  compact_null <- function(x) {
    Filter(function(elt) !is.null(elt), x)
  }

  args <- compact_null(args)
  n_args <- length(args)
  seq_args <- seq_len(n_args)

  # Validate and split each formula
  pairs <- vector("list", n_args)
  for (i in seq_args) {
    pairs[[i]] <- dplyr_validate_and_split_formula(
      x = args[[i]],
      i = i,
      default_env = default_env,
      dots_env = dots_env,
      error_call = error_call
    )
  }

  # Prepare lhs and rhs lists
  lhs <- vector("list", n_args)
  rhs <- vector("list", n_args)
  env_error_info <- rlang::env()

  withCallingHandlers(
    {
      for (i in seq_args) {
        env_error_info$i <- i
        pair <- pairs[[i]]

        env_error_info$side <- "left"
        elt_lhs <- rlang::eval_tidy(pair$lhs, env = default_env)

        env_error_info$side <- "right"
        elt_rhs <- rlang::eval_tidy(pair$rhs, env = default_env)

        if (!is.null(elt_lhs)) {
          lhs[[i]] <- elt_lhs
        }
        if (!is.null(elt_rhs)) {
          rhs[[i]] <- elt_rhs
        }
      }
    },
    error = function(cnd) {
      message <- glue::glue_data(env_error_info, "Failed to evaluate the {side}-hand side of formula {i}.")
      rlang::abort(message, parent = cnd, call = error_call)
    }
  )

  if (n_args > 0L) {
    names(lhs) <- paste0("..", seq_args, " (left)")
    names(rhs) <- paste0("..", seq_args, " (right)")
  }

  list(lhs = lhs, rhs = rhs)
}


# Adapt dplyr:::validate_and_split_formula() for dependence on rlang+vctrs+glue only
dplyr_validate_and_split_formula <- function(x, i, default_env, dots_env, error_call) {
  if (rlang::is_quosure(x)) {
    default_env <- rlang::quo_get_env(x)
    x <- rlang::quo_get_expr(x)
  }

  if (!rlang::is_formula(x, lhs = TRUE)) {
    arg_expr <- substitute(...(), dots_env)[[i]]
    arg_label <- glue::backtick(rlang::as_label(arg_expr))

    if (rlang::is_formula(x)) {
      type <- "a two-sided formula"
    } else {
      type <- glue::glue("a two-sided formula, not {dplyr_obj_type_friendly(x)}")
    }

    message <- glue::glue("Case {i} ({arg_label}) must be {type}.")
    rlang::abort(message, call = error_call)
  }

  env <- rlang::f_env(x) %||% default_env

  list(
    lhs = rlang::new_quosure(rlang::f_lhs(x), env),
    rhs = rlang::new_quosure(rlang::f_rhs(x), env)
  )
}


# Adapt dplyr:::vec_case_match() for dependence on rlang+vctrs+glue only
dplyr_vec_case_match <- function(needles, haystacks, values, ...,
                                needles_arg = "needles",
                                haystacks_arg = "haystacks",
                                values_arg = "values",
                                default = NULL,
                                default_arg = "default",
                                ptype = NULL,
                                call = rlang::current_env()) {

  rlang::check_dots_empty0(...)

  vctrs::obj_check_vector(needles, arg = needles_arg, call = call)
  vctrs::obj_check_list(haystacks, arg = haystacks_arg, call = call)
  vctrs::list_check_all_vectors(haystacks, arg = haystacks_arg, call = call)

  # Cast haystacks to the same type as needles
  haystacks <- vctrs::vec_cast_common(!!!haystacks, .to = needles, .arg = haystacks_arg, .call = call)

  # Create list of logical vectors: whether each haystack value is in needles
  conditions <- lapply(haystacks, vctrs::vec_in, needles = needles)

  size <- vctrs::vec_size(needles)

  dplyr_vec_case_when(
    conditions = conditions,
    values = values,
    conditions_arg = "",  # internal use
    values_arg = values_arg,
    default = default,
    default_arg = default_arg,
    ptype = ptype,
    size = size,
    call = call
  )
}


# Adapt dplyr:::vec_case_when() for dependence on rlang+vctrs+glue only
dplyr_vec_case_when <- function(conditions, values, ...,
                               conditions_arg = "conditions",
                               values_arg = "values",
                               default = NULL,
                               default_arg = "default",
                               ptype = NULL,
                               size = NULL,
                               call = rlang::current_env()) {

  # Validate input
  rlang::check_dots_empty0(...)

  vctrs::obj_check_list(conditions, arg = "conditions", call = call)
  vctrs::obj_check_list(values, arg = "values", call = call)

  vctrs::list_check_all_vectors(values, arg = values_arg, call = call)

  n_conditions <- length(conditions)
  n_values <- length(values)

  if (n_conditions != n_values) {
    message <- glue::glue(
      "The number of supplied conditions ({n_conditions}) must equal ",
      "the number of supplied values ({n_values})."
    )
    rlang::abort(message, call = call)
  }

  # if (n_conditions == 0L) {
  #   rlang::abort("At least one condition must be supplied.", call = call)
  # }

  if (!rlang::is_string(conditions_arg)) {
    rlang::abort("`conditions_arg` must be a string.", call = call)
  }
  if (!rlang::is_string(values_arg)) {
    rlang::abort("`values_arg` must be a string.", call = call)
  }
  if (!rlang::is_string(default_arg)) {
    rlang::abort("`default_arg` must be a string.", call = call)
  }

  condition_args <- rlang::names2(conditions)
  condition_args <- dplyr_names_as_error_names(condition_args, arg = conditions_arg)

  value_args <- rlang::names2(values)
  value_args <- dplyr_names_as_error_names(value_args, arg = values_arg)

  names(conditions) <- condition_args
  names(values) <- value_args

  for (i in seq_len(n_conditions)) {
    condition <- conditions[[i]]
    condition_arg <- condition_args[[i]]
    dplyr_check_logical(condition, arg = condition_arg, call = call)
  }

  size <- vctrs::vec_size_common(!!!conditions, .size = size, .call = call)

  extras <- list(default)
  names(extras) <- default_arg
  everything <- c(values, extras)

  ptype <- vctrs::vec_ptype_common(!!!everything, .ptype = ptype, .call = call)

  values <- vctrs::vec_cast_common(!!!values, .to = ptype, .call = call)

  if (is.null(default)) {
    default <- vctrs::vec_init(ptype)
  } else {
    default <- vctrs::vec_cast(x = default, to = ptype, x_arg = default_arg, call = call)
  }

  for (i in seq_len(n_conditions)) {
    condition <- conditions[[i]]
    condition_arg <- condition_args[[i]]
    vctrs::vec_check_size(condition, size = size, arg = condition_arg, call = call)
  }

  value_sizes <- vctrs::list_sizes(values)

  for (i in seq_len(n_values)) {
    value_size <- value_sizes[[i]]
    if (value_size != 1L) {
      value <- values[[i]]
      value_arg <- value_args[[i]]
      vctrs::vec_check_size(value, size = size, arg = value_arg, call = call)
    }
  }

  default_size <- vctrs::vec_size(default)
  if (default_size != 1L) {
    vctrs::vec_check_size(default, size = size, arg = default_arg, call = call)
  }

  n_processed <- 0L
  locs <- vector("list", n_values)
  are_unused <- vctrs::vec_rep(TRUE, times = size)

  for (i in seq_len(n_conditions)) {
    if (!any(are_unused)) {
      break
    }
    condition <- conditions[[i]]
    loc <- are_unused & condition
    loc <- which(loc)
    locs[[i]] <- loc
    are_unused[loc] <- FALSE
    n_processed <- n_processed + 1L
  }

  if (n_processed == n_conditions && any(are_unused)) {
    loc_unused <- which(are_unused)
    n_processed <- n_processed + 1L
    n_values <- n_values + 1L
    locs[[n_values]] <- loc_unused
    values[[n_values]] <- default
    value_sizes[[n_values]] <- default_size
  }

  for (i in seq_len(n_processed)) {
    loc <- locs[[i]]
    value <- values[[i]]
    value_size <- value_sizes[[i]]

    if (value_size == 1L) {
      value <- vctrs::vec_recycle(value, size = vctrs::vec_size(loc))
    } else {
      value <- vctrs::vec_slice(value, loc)
    }
    values[[i]] <- value
  }

  values <- unname(values)

  if (n_processed != n_values) {
    seq_processed <- seq_len(n_processed)
    values <- values[seq_processed]
    locs <- locs[seq_processed]
  }

  vctrs::list_unchop(x = values, indices = locs, ptype = ptype)
}


# Adapt dplyr:::names_as_error_names() for dependence on rlang+vctrs+glue only
dplyr_names_as_error_names <- function(names, arg = "") {
  unnamed <- names == ""

  if (arg == "") {
    loc_unnamed <- which(unnamed)
    names[loc_unnamed] <- dplyr_vec_paste0("..", loc_unnamed)
  } else {
    loc_named <- which(!unnamed)
    loc_unnamed <- which(unnamed)

    names[loc_named] <- dplyr_vec_paste0(arg, "$", names[loc_named])
    names[loc_unnamed] <- dplyr_vec_paste0(arg, "[[", loc_unnamed, "]]")
  }

  names
}


# Adapt dplyr:::vec_paste0() for dependence on rlang+vctrs+glue only
dplyr_vec_paste0 <- function(...) {
  args <- vctrs::vec_recycle_common(...)
  rlang::exec(paste0, !!!args)
}

# Adapt dplyr:::check_logical() for dependence on rlang+vctrs+glue only
# (This would be a good candidate to swap out for zmisc::assert_logical())
dplyr_check_logical <- function(x, ..., allow_null = FALSE, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!missing(x)) {
    if (rlang::is_logical(x)) {
      return(invisible(NULL))
    }
    if (allow_null && rlang::is_null(x)) {
      return(invisible(NULL))
    }
  }

  # Manual version of stop_input_type
  expected <- "a logical vector"
  actual <- dplyr_obj_type_friendly(x)
  msg <- glue::glue("`{arg}` must be {expected}, not {actual}.")

  rlang::abort(msg, call = call)
}


dplyr_obj_type_friendly <- function(x, value = TRUE) {
  if (rlang::is_missing(x)) {
    return("absent")
  }

  if (is.object(x)) {
    if (inherits(x, "quosure")) {
      type <- "quosure"
    } else {
      type <- paste(class(x), collapse = "/")
    }
    return(sprintf("a <%s> object", type))
  }

  if (!rlang::is_vector(x)) {
    return(dplyr_dot_rlang_as_friendly_type(x))
  }

  n_dim <- length(dim(x))

  if (!n_dim) {
    if (!rlang::is_list(x) && length(x) == 1) {
      if (vctrs::vec_equal_na(x)) {
        # Handle NA types
        return(switch(typeof(x),
                      "logical"   = "`NA`",
                      "integer"   = "an integer `NA`",
                      "double"    = if (is.nan(x)) "`NaN`" else "a numeric `NA`",
                      "complex"   = "a complex `NA`",
                      "character" = "a character `NA`",
                      rlang::abort("Unexpected typeof for NA value.", call = rlang::caller_env())
        ))
      }

      show_infinites <- function(x) {
        if (x > 0) "`Inf`" else "`-Inf`"
      }

      str_encode <- function(x, width = 30, ...) {
        if (nchar(x) > width) {
          x <- substr(x, 1, width - 3)
          x <- paste0(x, "...")
        }
        encodeString(x, ...)
      }

      if (value) {
        if (is.numeric(x) && is.infinite(x)) {
          return(show_infinites(x))
        }
        if (is.numeric(x) || is.complex(x)) {
          number <- as.character(round(x, 2))
          what <- if (is.complex(x)) "the complex number" else "the number"
          return(paste(what, number))
        }
        return(switch(typeof(x),
                      "logical" = if (x) "`TRUE`" else "`FALSE`",
                      "character" = {
                        what <- if (nzchar(x)) "the string" else "the empty string"
                        paste(what, str_encode(x, quote = "\""))
                      },
                      "raw" = paste("the raw value", as.character(x)),
                      rlang::abort("Unexpected typeof for scalar value.", call = rlang::caller_env())
        ))
      }

      return(switch(typeof(x),
                    "logical" = "a logical value",
                    "integer" = "an integer",
                    "double"  = if (is.infinite(x)) show_infinites(x) else "a number",
                    "complex" = "a complex number",
                    "character" = if (nzchar(x)) "a string" else "\"\"",
                    "raw" = "a raw value",
                    rlang::abort("Unexpected typeof for scalar type.", call = rlang::caller_env())
      ))
    }

    if (length(x) == 0) {
      return(switch(typeof(x),
                    "logical" = "an empty logical vector",
                    "integer" = "an empty integer vector",
                    "double"  = "an empty numeric vector",
                    "complex" = "an empty complex vector",
                    "character" = "an empty character vector",
                    "raw" = "an empty raw vector",
                    "list" = "an empty list",
                    rlang::abort("Unexpected typeof for empty vector.", call = rlang::caller_env())
      ))
    }
  }

  dplyr_vec_type_friendly(x)
}


dplyr_vec_type_friendly <- function(x, length = FALSE) {
  if (!rlang::is_vector(x)) {
    rlang::abort("`x` must be a vector.")
  }

  type <- typeof(x)
  n_dim <- length(dim(x))

  add_length <- function(type_string) {
    if (length && n_dim == 0) {
      paste0(type_string, sprintf(" of length %s", length(x)))
    } else {
      type_string
    }
  }

  if (type == "list") {
    if (n_dim < 2) {
      return(add_length("a list"))
    } else if (is.data.frame(x)) {
      return("a data frame")
    } else if (n_dim == 2) {
      return("a list matrix")
    } else {
      return("a list array")
    }
  }

  type_string <- switch(type,
                        "logical"   = "a logical %s",
                        "integer"   = "an integer %s",
                        "numeric"   = "a double %s",  # numeric = double
                        "double"    = "a double %s",
                        "complex"   = "a complex %s",
                        "character" = "a character %s",
                        "raw"       = "a raw %s",
                        paste0("a ", type, " %s")     # fallback
  )

  kind <- if (n_dim == 0) {
    "vector"
  } else if (n_dim == 2) {
    "matrix"
  } else {
    "array"
  }

  out <- sprintf(type_string, kind)

  if (n_dim >= 2) {
    out
  } else {
    add_length(out)
  }
}

dplyr_dot_rlang_as_friendly_type <- function (type)
{
  switch(type, list = "a list", `NULL` = "`NULL`", environment = "an environment",
         externalptr = "a pointer", weakref = "a weak reference",
         S4 = "an S4 object", name = , symbol = "a symbol", language = "a call",
         pairlist = "a pairlist node", expression = "an expression vector",
         char = "an internal string", promise = "an internal promise",
         ... = "an internal dots object", any = "an internal `any` object",
         bytecode = "an internal bytecode object", primitive = ,
         builtin = , special = "a primitive function", closure = "a function",
         type)
}
