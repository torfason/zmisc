
#' Glue interpolation vectors in pipes
#'
#' Applies `glue::glue()` to each element of a character vector using a template
#' string, enabling pipe-friendly, element-wise interpolation. Useful when the
#' vector to process is not encapsulated in a `data.frame` or other
#' environment-like object.
#'
#' @param . A character vector to be interpolated.
#' @param template A glue template string. Use `{.}` to refer to the default
#'   (unnamed) vector variable, or the names of any other variables accessible
#'   in the relevant environment. Variables are recycled using tidyverse
#'   recycling rules.
#' @param ... Reserved and should not be used.
#' @param .sep,.envir,.open,.close,.na,.null,.comment,.literal,.transformer,.trim
#'   Arguments passed on to [glue::glue()]. Must be passed by name.
#'   See [glue::glue()] for details.
#'
#' @return A character vector with interpolated values. The length is determined
#'   by tidyverse recycling rules for all referenced variables.
#'
#' @examples
#'   letters |> glue_vector("Letters include {.} and {LETTERS}")
#'
#' @export
glue_vector <- function(., template = "{.}", ...,
      .sep = "", .envir = parent.frame(), .open = "{", .close = "}",
      .na = "NA", .null = character(), .comment = "#", .literal = FALSE,
      .transformer = glue::identity_transformer, .trim = TRUE) {

  # Check inputs
  checkmate::assert_atomic_vector(.)
  chk_dots_empty()

  # Call glue with arg <.> embedded in a list
  glue::glue_data(template, .x = list(. = .),
          .sep = .sep, .envir = .envir, .open = .open, .close = .close,
          .na = .na, .null = .null, .comment = .comment, .literal = .literal,
          .transformer = .transformer, .trim = .trim)
}

