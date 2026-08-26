
## asciify.R ------------------------------------------------------------
##
## Base R implementation of asciify(), with the substitution table built once
## at package build time rather than on every call.
##
## Keep the constants and the function in the SAME source file. Top-level code
## in R/ is evaluated at install time in collate order, and .asciify_multi
## depends on .asciify_ascii, so splitting them across files would make
## correctness depend on filename ordering.


## ---- substitution table, evaluated at build time --------------------------
##
## Stored as code points so this file stays pure ascii and needs no
## unescaping step. intToUtf8() runs once at install; the resulting
## UTF-8-marked strings are serialized into the package namespace.

.asciify_org <- intToUtf8(c(
  0x00c1, 0x00e1, 0x00d0, 0x00f0, 0x00c9, 0x00e9, 0x00cd, 0x00ed, 0x00d3, 0x00f3,
  0x00da, 0x00fa, 0x00dd, 0x00fd, 0x00de, 0x00fe, 0x00c6, 0x00e6, 0x00d6, 0x00f6,
  0x00c5, 0x00e5, 0x00c4, 0x00e4, 0x00d8, 0x00f8, 0x00df, 0x00c0, 0x00e0, 0x00c2,
  0x00e2, 0x00c7, 0x00e7, 0x00c8, 0x00e8, 0x00ca, 0x00ea, 0x00ce, 0x00ee, 0x00cf,
  0x00ef, 0x00d4, 0x00f4, 0x0152, 0x0153, 0x00d9, 0x00f9, 0x00db, 0x00fb, 0x00dc,
  0x00fc, 0x0178, 0x00ff, 0x00d1, 0x00f1, 0x00cc, 0x00ec, 0x00d2, 0x00f2, 0x00d5,
  0x00f5, 0x0100, 0x0101, 0x0112, 0x0113, 0x0122, 0x0123, 0x012a, 0x012b, 0x0136,
  0x0137, 0x013b, 0x013c, 0x0145, 0x0146, 0x016a, 0x016b, 0x010c, 0x010d, 0x0160,
  0x0161, 0x017d, 0x017e, 0x0104, 0x0105, 0x0118, 0x0119, 0x0116, 0x0117, 0x012e,
  0x012f, 0x0172, 0x0173, 0x0106, 0x0107, 0x0141, 0x0142, 0x0143, 0x0144, 0x015a,
  0x015b, 0x0179, 0x017a, 0x017b, 0x017c, 0x0150, 0x0151, 0x0170, 0x0171, 0x010e,
  0x010f, 0x011a, 0x011b, 0x0147, 0x0148, 0x0158, 0x0159, 0x0164, 0x0165, 0x016e,
  0x016f, 0x0139, 0x013a, 0x013d, 0x013e, 0x0154, 0x0155, 0x010a, 0x010b, 0x0120,
  0x0121, 0x0126, 0x0127, 0x0102, 0x0103, 0x0218, 0x0219, 0x021a, 0x021b, 0x00cb,
  0x00eb, 0x0110, 0x0111), multiple = TRUE)

## The most appropriate ascii representations of the non-ascii characters.
## Nine of these are one-to-many: Th th Ae ae ss OE oe Ue ue.
.asciify_ascii <- c("A", "a", "D", "d", "E", "e", "I", "i", "O", "o", "U", "u", "Y",
                    "y", "Th", "th", "Ae", "ae", "O", "o", "A", "a", "A", "a", "O", "o", "ss",
                    "A", "a", "A", "a", "C", "c", "E", "e", "E", "e", "I", "i", "I", "i", "O",
                    "o", "OE", "oe", "U", "u", "U", "u", "Ue", "ue", "Y", "y", "N", "n", "I", "i",
                    "O", "o", "O", "o", "A", "a", "E", "e", "G", "g", "I", "i", "K", "k", "L",
                    "l", "N", "n", "U", "u", "C", "c", "S", "s", "Z", "z", "A", "a", "E", "e",
                    "E", "e", "I", "i", "U", "u", "C", "c", "L", "l", "N", "n", "S", "s", "Z",
                    "z", "Z", "z", "O", "o", "U", "u", "D", "d", "E", "e", "N", "n", "R", "r",
                    "T", "t", "U", "u", "L", "l", "L", "l", "R", "r", "C", "c", "G", "g", "H",
                    "h", "A", "a", "S", "s", "T", "t", "E", "e", "D", "d")
stopifnot(length(.asciify_org) == length(.asciify_ascii))

## Indices of the one-to-many mappings, which need gsub()
.asciify_multi <- which(nchar(.asciify_ascii) > 1L)

## The one-to-one mappings, precollapsed for a single chartr() pass.
## chartr() reads "a-b" in `old` as a range; none of these characters is a
## hyphen, and the stopifnot() below guards against one being added later.
.asciify_old <- paste(.asciify_org[-.asciify_multi],   collapse = "")
.asciify_new <- paste(.asciify_ascii[-.asciify_multi], collapse = "")
stopifnot(!grepl("-", .asciify_old, fixed = TRUE))
stopifnot(nchar(.asciify_old) == nchar(.asciify_new))

## Character class matching any of the one-to-many source characters, used to
## find the (usually few) elements that need the gsub() loop at all. None of
## the nine is a regex metacharacter, so plain concatenation inside [] is safe.
.asciify_re <- paste0("[", paste(.asciify_org[.asciify_multi], collapse = ""), "]")



#' Convert non-ASCII characters to their ASCII equivalents
#'
#' This function replaces non-ASCII characters in a string with their ASCII
#' equivalents. It supports a range of European non-ASCII characters, including
#' Icelandic, Swedish, Norwegian, Danish, Finnish, German, Estonian, Latvian,
#' Lithuanian, Polish, Hungarian, Slovenian, Czech, Slovak, Maltese, Romanian,
#' Albanian, and Croatian.
#'
#' @param x A character vector to be processed.
#'
#' @param verify A logical value indicating whether to verify that the result is
#'   ASCII. Defaults to `TRUE`. If `FALSE`, the function will not check that the
#'   result is ASCII and it may return non-ASCII characters.
#'
#' @return A character vector with non-ASCII characters replaced by their ASCII
#'   equivalents.
#'
#' @examples
#' asciify("Jón Þór Birgisson") # "Jon Thor Birgisson"
#' asciify("förståndshandikapp") # "forstandshandikapp"
#' asciify("Viðareiði") # "Vidareidi"
#' asciify("übermensch") # "uebermensch"
#' asciify("Jürgen Klopp") # "Juergen Klopp"
#' asciify("rõõmsameelsus") # "roomsameelsus"
#' asciify("Mężczyzna") # "Mezczyzna"
#' asciify("Škoda") # "Skoda"
#'
#' @md
#' @export
asciify <- function(x, verify = TRUE) {

  # Sanity
  stopifnot(is.character(x), is.null(dim(x)))
  stopifnot(is.logical(verify), length(verify) == 1L, !is.na(verify))

  result <- x

  # One-to-many replacements, applied only to the elements that contain at
  # least one of the nine source characters. One regex scan replaces nine
  # full-vector rebuilds; NA never matches and falls through to chartr().
  hits <- grep(.asciify_re, result)
  if (length(hits)) {
    chunk <- result[hits]
    for (i in .asciify_multi) {
      chunk <- gsub(.asciify_org[i], .asciify_ascii[i], chunk, fixed = TRUE)
    }
    result[hits] <- chunk
  }

  # The remaining one-to-one replacements are a single chartr() pass
  result <- chartr(.asciify_old, .asciify_new, result)

  # Verify the result (any byte >= 128 means something was left untranslated)
  if (verify) {
    if (any(grepl("[^\x01-\x7f]", result, useBytes = TRUE), na.rm = TRUE)) {
      stop(paste0(
        "The result of asciify() still contains non-ascii characters. \n",
        "  The input probably contains characters that are not handled \n",
        "  by the function. Use 'verify = FALSE' if this is what you want."
      ))
    }
  }

  # Return the result
  result
}
