## yencode.R -----------------------------------------------------------------
##
## Base R rewrite of yencode()/yencoder(), plus a faster ydecode().
## No stringr dependency.
##
## Design note: the encoding is a pure byte-to-string mapping. Rather than
## building a regex character class and testing every character against it,
## the whitelist is compiled once into a 256-entry lookup vector, where
## element i+1 is the output for byte value i -- either the literal byte or
## its escape sequence. Encoding a string is then one charToRaw(), one
## vector index, and one paste0(). This removes the need to regex-escape
## the whitelist at all.


## ---- internal: compile the whitelist into a byte lookup table -------------

#' Compile an encoding table for yencode
#'
#' @param escape The escape character to use.
#' @param whitelist Any characters that should not be escaped.
#' @return A list with the byte lookup table and the multi-byte whitelist
#'   entries that need restoring after the byte pass.
#'
#' @keywords internal
.yencode_map <- function(escape = "%",
                         whitelist = c("._~-", "][!$&'()*+,;=:/?@#")) {

  # The escape string must be one single-byte character
  stopifnot( is.character(escape) ,
             length(escape) == 1 ,
             length(charToRaw(escape)) == 1 )

  # Treat NULL or NA whitelist as empty string, but other types should error
  if (is.null(whitelist)) { whitelist <- "" }
  whitelist[is.na(whitelist)] <- ""
  stopifnot(is.character(whitelist))

  # The following characters are always whitelisted and cannot be escaped
  whitelist_core <- "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  whitelist_final <- paste0(whitelist_core, paste0(whitelist, collapse = ""))

  # The escape character must NOT be in the final whitelist.
  # NOTE: gsub() here, not sub(). str_remove() dropped only the first
  # occurrence, so a whitelist naming the escape character twice left the
  # second one in place and tripped the stop() below instead.
  if (grepl(escape, whitelist_final, fixed = TRUE)) {
    warning(paste0("The escape character (", escape, ") cannot be present in the whitelist,\n",
                   "and has been removed. The results are, strictly speaking, well-specified\n",
                   "and robust, but you should be sure you know what you are doing."))
    whitelist_final <- gsub(escape, "", whitelist_final, fixed = TRUE)
  }

  # Split into characters, not bytes, so multi-byte whitelist entries survive
  wl_chars <- unique(strsplit(whitelist_final, "")[[1L]])
  wl_width <- nchar(wl_chars, type = "bytes")

  # Defensive programming: the escape character byte must not be an intra-char
  # byte in any of the whitelist chars
  wl_bytes <- charToRaw(paste0(wl_chars, collapse = ""))
  if (any(wl_bytes == charToRaw(escape))) {
    stop(paste0("The escape character byte (", charToRaw(escape), ") is present within,\n",
                "a character in this whitelist. The offending whitelist character\n",
                "must be removed manually."))
  }

  # Byte lookup: escape sequence by default, literal for whitelisted bytes
  map  <- paste0(escape, toupper(as.character(as.raw(0:255))))
  keep <- as.integer(charToRaw(paste0(wl_chars[wl_width == 1L], collapse = "")))
  map[keep + 1L] <- rawToChar(as.raw(keep), multiple = TRUE)

  # Multi-byte whitelist characters cannot be expressed in a byte table
  # (whitelisting their individual bytes would also pass those bytes through
  # inside other characters). They are escaped by the byte pass and restored
  # afterwards. UTF-8 is prefix-free, so the restorations cannot overlap.
  wide    <- wl_chars[wl_width > 1L]
  encoded <- vapply(wide,
                    function(ch) paste0(map[as.integer(charToRaw(ch)) + 1L], collapse = ""),
                    character(1), USE.NAMES = FALSE)

  list(map = map, wide = wide, encoded = encoded)
}


## ---- internal: apply a compiled table -------------------------------------

#' @keywords internal
.yencode_apply <- function(string, m) {

  string <- as.character(string)

  out <- vapply(string, function(s) {
    if (is.na(s)) return(NA_character_)
    paste0(m$map[as.integer(charToRaw(s)) + 1L], collapse = "")
  }, character(1), USE.NAMES = FALSE)

  for (i in seq_along(m$wide)) {
    out <- gsub(m$encoded[i], m$wide[i], out, fixed = TRUE)
  }

  out
}


## ---- yencode --------------------------------------------------------------

#' Yet (another urlencode compatible) encoding scheme
#'
#' @param string The string to process.
#' @param escape The escape character to use.
#' @param whitelist Any characters that should not be escaped. See details.
#' @return The processed (encoded or decoded) string.
#'
#' @md
#' @export
yencode <- function(string, escape = "%",
                    whitelist = c("._~-", "][!$&'()*+,;=:/?@#")) {
  .yencode_apply(string, .yencode_map(escape, whitelist))
}


#' @rdname yencode
#' @md
#' @export
yencoder <- function(escape = "%",
                     whitelist = c("._~-", "][!$&'()*+,;=:/?@#")) {

  # The table is compiled once, here. The "escape in whitelist" warning is
  # therefore raised exactly once, on creation, which is what the old
  # suppress_warnings() call in the returned closure was working around.
  m <- .yencode_map(escape, whitelist)

  function(string) .yencode_apply(string, m)
}


## ---- ydecode --------------------------------------------------------------

#' @rdname yencode
#' @md
#' @export
ydecode <- function(string, escape = "%") {

  stopifnot( is.character(escape) ,
             length(escape) == 1 ,
             length(charToRaw(escape)) == 1 )

  pc <- charToRaw(escape)

  vapply(as.character(string), function(s) {

    if (is.na(s)) return(NA_character_)

    x <- charToRaw(s)
    n <- length(x)
    if (n == 0L) return("")

    cand <- which(x == pc)
    if (length(cand) == 0L) return(s)

    # An escape at position p consumes p, p+1 and p+2, so escape bytes that
    # fall inside an earlier sequence are literal hex digits. The scan is
    # sequential but runs over escape positions only, not over every byte.
    starts <- integer(length(cand))
    k    <- 0L
    used <- 0L
    for (p in cand) {
      if (p > used) {
        k <- k + 1L
        starts[k] <- p
        used <- p + 2L
      }
    }
    starts <- starts[seq_len(k)]

    if (starts[k] + 2L > n) {
      stop("Truncated escape sequence at the end of the input string.")
    }

    # strtoi() replaces the manual hex arithmetic and accepts either case
    hex <- paste0(rawToChar(x[starts + 1L], multiple = TRUE),
                  rawToChar(x[starts + 2L], multiple = TRUE))
    val <- strtoi(hex, 16L)
    if (anyNA(val)) stop("Malformed escape sequence in the input string.")

    x[starts] <- as.raw(val)
    rawToChar(x[-c(starts + 1L, starts + 2L)])

  }, character(1), USE.NAMES = FALSE)
}


#' @rdname yencode
#' @md
#' @export
ydecoder <- function(escape = "%") {
  function(string) ydecode(string, escape)
}
