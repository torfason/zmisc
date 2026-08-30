
## Re-exports -----------------------------------------------------------
##
## glue() and glue_data() are re-exported so that a user who has attached
## zmisc for glue_vector() also has the two functions it is built on, without
## needing to attach glue as well. roxygen2 collects both into the generated
## man/reexports.Rd.

#' @importFrom glue glue
#' @export
glue::glue

#' @importFrom glue glue_data
#' @export
glue::glue_data


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

  # Check inputs.
  #  - chk_atomic() accepts arrays, so the dim guard that
  #    checkmate::assert_atomic_vector() used to provide has no chk_*()
  #    equivalent and is kept explicitly
  #  - Arguments passed directly to glue_data() are not checked here,
  #    to avoid duplication. They will fail in glue_data() with relevant
  #    error messages.
  chk_atomic(.)
  is.null(dim(.)) || stop("`.` must not have a dim attribute")
  chk_string(template)
  chk_dots_empty()

  # The remaining arguments are handed straight to glue::glue_data().

  # Call glue with arg <.> embedded in a list
  glue::glue_data(template, .x = list(. = .),
          .sep = .sep, .envir = .envir, .open = .open, .close = .close,
          .na = .na, .null = .null, .comment = .comment, .literal = .literal,
          .transformer = .transformer, .trim = .trim)
}

