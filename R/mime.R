# RFC 2047 encoded words ("=?charset?Q?...?=" and "=?charset?B?...?="). They are
# decoded here because mRpostman::decode_mime_header() fails on a word inside
# other text ("Display =?UTF-8?Q?Name?= <a@b>"), mixes up adjacent Q and B words
# and returns "NA" for NA.
mime_encoded_word_re <- "=\\?([^?[:space:]]+)\\?([QqBb])\\?([^?]*)\\?="

decode_mime_header <- function(x) {
  if (is.na(x) || !nzchar(x)) {
    return(x)
  }

  # RFC 2047 sec 2: whitespace *between* adjacent encoded-words is not part
  # of the decoded text.
  x <- gsub(paste0("(", mime_encoded_word_re, ")[ \t]+(?==\\?)"), "\\1", x, perl = TRUE)

  m <- gregexpr(mime_encoded_word_re, x, perl = TRUE)[[1]]
  if (m[1] == -1L) {
    return(x)
  }

  lens <- attr(m, "match.length")
  out <- character(0)
  cursor <- 1L
  for (i in seq_along(m)) {
    if (m[i] > cursor) {
      out <- c(out, substr(x, cursor, m[i] - 1L))
    }
    whole <- substr(x, m[i], m[i] + lens[i] - 1L)
    parts <- regmatches(whole, regexec(mime_encoded_word_re, whole, perl = TRUE))[[1]]
    out <- c(out, decode_encoded_word(charset = parts[2], enc = parts[3], text = parts[4]))
    cursor <- m[i] + lens[i]
  }
  if (cursor <= nchar(x)) {
    out <- c(out, substr(x, cursor, nchar(x)))
  }
  paste(out, collapse = "")
}

decode_encoded_word <- function(charset, enc, text) {
  raw <- if (toupper(enc) == "B") base64_decode(text) else qp_decode(text, space_as_underscore = TRUE)
  bytes_to_string(raw, charset)
}

bytes_to_string <- function(raw, charset) {
  raw <- raw[raw != as.raw(0L)] # rawToChar() errors on an embedded nul
  if (length(raw) == 0L) {
    return("")
  }
  s <- rawToChar(raw)
  charset <- toupper(charset %||% "UTF-8")

  if (!charset %in% c("UTF-8", "UTF8", "US-ASCII", "ASCII")) {
    converted <- tryCatch(iconv(s, from = charset, to = "UTF-8"), error = function(e) NA_character_)
    if (!is.na(converted)) {
      return(converted)
    }
  }

  # Declared UTF-8/ASCII, or an unknown charset: trust UTF-8 only if the bytes
  # really are valid UTF-8 — mislabeled Latin-1 is the usual culprit when not.
  Encoding(s) <- "UTF-8"
  if (validUTF8(s)) {
    return(s)
  }
  # A body cut by `body_limit` can end inside a character: drop those bytes
  # instead of reading the whole text as Latin-1.
  trimmed <- drop_incomplete_utf8_tail(raw)
  if (length(trimmed) < length(raw)) {
    t <- rawToChar(trimmed)
    Encoding(t) <- "UTF-8"
    if (validUTF8(t)) {
      return(t)
    }
  }
  Encoding(s) <- "unknown"
  iconv(s, from = "latin1", to = "UTF-8")
}

# Removes a final UTF-8 character that is cut short: a lead byte at the end
# without all of its continuation bytes. Anything else is left alone.
drop_incomplete_utf8_tail <- function(raw) {
  n <- length(raw)
  for (back in 0:min(2L, n - 1L)) {
    j <- n - back
    b <- as.integer(raw[j])
    if (b < 0x80L) {
      return(raw) # ASCII: nothing is cut
    }
    if (b >= 0xC0L) { # a lead byte: how many bytes does it promise?
      need <- if (b >= 0xF0L) 4L else if (b >= 0xE0L) 3L else 2L
      return(if (n - j + 1L < need) raw[seq_len(j - 1L)] else raw)
    }
  }
  raw
}

# Quoted-printable decoding of raw bytes: "=XX" becomes that byte, "_" a space
# (in Q-encoded words only) and a soft line break ("=" at end of line) is
# removed. Vectorised, so linear in the text length, and multibyte characters
# stay intact.
qp_decode <- function(text, space_as_underscore) {
  text <- gsub("=(\r\n|\n|\r)", "", text)
  bytes <- charToRaw(text)
  if (space_as_underscore) {
    bytes[bytes == as.raw(0x5fL)] <- as.raw(0x20L)
  }

  n <- length(bytes)
  eq <- which(bytes == as.raw(0x3dL))
  eq <- eq[eq + 2L <= n]
  if (length(eq) == 0L) {
    return(bytes)
  }

  hex_value <- function(b) {
    v <- as.integer(b)
    ifelse(v >= 48L & v <= 57L, v - 48L,
      ifelse(v >= 65L & v <= 70L, v - 55L,
        ifelse(v >= 97L & v <= 102L, v - 87L, NA_integer_)
      )
    )
  }
  hi <- hex_value(bytes[eq + 1L])
  lo <- hex_value(bytes[eq + 2L])
  ok <- !is.na(hi) & !is.na(lo)
  if (!any(ok)) {
    return(bytes)
  }

  eq <- eq[ok]
  bytes[eq] <- as.raw(hi[ok] * 16L + lo[ok])
  bytes[-c(eq + 1L, eq + 2L)]
}

base64_alphabet <- c(LETTERS, letters, as.character(0:9), "+", "/")

base64_lookup <- stats::setNames(0:63, base64_alphabet)

base64_decode <- function(text) {
  text <- gsub("[^A-Za-z0-9+/=]", "", text)
  text <- sub("=+$", "", text)
  n <- nchar(text)
  if (n == 0L) {
    return(raw(0))
  }
  vals <- unname(base64_lookup[strsplit(text, "", fixed = TRUE)[[1]]])
  vals <- c(vals, rep(0L, (-n) %% 4L))
  m <- matrix(vals, nrow = 4L)

  b1 <- bitwOr(bitwShiftL(m[1L, ], 2L), bitwShiftR(m[2L, ], 4L))
  b2 <- bitwOr(bitwShiftL(bitwAnd(m[2L, ], 15L), 4L), bitwShiftR(m[3L, ], 2L))
  b3 <- bitwOr(bitwShiftL(bitwAnd(m[3L, ], 3L), 6L), m[4L, ])

  as.raw(as.vector(rbind(b1, b2, b3)))[seq_len((n * 6L) %/% 8L)]
}

# One field's raw, still encoded value from a block of raw headers, as
# ImapCon$fetch_header() returns it, after unfolding continuation lines
# (RFC 5322).
extract_header_field <- function(raw_header, field) {
  if (is.null(raw_header) || length(raw_header) == 0L) {
    return(NA_character_)
  }
  block <- paste(raw_header, collapse = "\n")
  if (!nzchar(block)) {
    return(NA_character_)
  }
  unfolded <- gsub("(\r\n|\n)[ \t]+", " ", block)
  lines <- strsplit(unfolded, "\r\n|\n")[[1]]
  hit <- grep(paste0("^", field, ":"), lines, ignore.case = TRUE, value = TRUE)
  if (length(hit) == 0L) {
    return(NA_character_)
  }
  trimws(sub(paste0("^", field, ":"), "", hit[1], ignore.case = TRUE))
}

zone_offsets <- c(
  UT = 0, UTC = 0, GMT = 0, Z = 0,
  EST = -5, EDT = -4, CST = -6, CDT = -5, MST = -7, MDT = -6, PST = -8, PDT = -7
)

# Parses RFC 5322 dates ("Mon, 7 May 2001 08:41:00 -0700"; weekday and seconds
# optional, numeric or named zone) to POSIXct in UTC, then plain ISO timestamps;
# anything else is NA. Parsed by hand because strptime() month and weekday names
# follow the session locale, and as.POSIXct() with several formats errors on the
# first unparseable value.
parse_mail_date <- function(x) {
  x <- as.character(x)
  pattern <- paste0(
    "^\\s*(?:[A-Za-z]{3},?\\s*)?(\\d{1,2})\\s+([A-Za-z]{3})[A-Za-z]*\\s+(\\d{2,4})",
    "\\s+(\\d{1,2}):(\\d{2})(?::(\\d{2}))?\\s*([+-]\\d{4}|[A-Za-z]{1,4})?"
  )
  m <- regmatches(x, regexec(pattern, x, perl = TRUE))

  out <- vapply(m, function(g) {
    if (length(g) == 0L) {
      return(NA_real_)
    }
    month <- match(tolower(g[3L]), tolower(month_abb_en))
    year <- as.integer(g[4L])
    if (is.na(month)) {
      return(NA_real_)
    }
    if (year < 50L) year <- year + 2000L else if (year < 100L) year <- year + 1900L
    secs <- if (nzchar(g[7L])) as.integer(g[7L]) else 0L
    utc <- as.numeric(ISOdatetime(
      year, month, as.integer(g[2L]), as.integer(g[5L]), as.integer(g[6L]), secs,
      tz = "UTC"
    ))
    utc - zone_seconds(g[8L])
  }, numeric(1))

  # anything the RFC pattern missed may still be a plain ISO timestamp
  for (fmt in c("%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d")) {
    todo <- is.na(out) & !is.na(x)
    if (!any(todo)) break
    out[todo] <- as.numeric(as.POSIXct(strptime(x[todo], fmt, tz = "UTC")))
  }
  as.POSIXct(out, origin = "1970-01-01", tz = "UTC")
}

zone_seconds <- function(z) {
  if (is.na(z) || !nzchar(z)) {
    return(0)
  }
  if (grepl("^[+-]\\d{4}$", z)) {
    sign <- if (substr(z, 1L, 1L) == "-") -1 else 1
    return(sign * (as.integer(substr(z, 2L, 3L)) * 3600 + as.integer(substr(z, 4L, 5L)) * 60))
  }
  offset <- zone_offsets[toupper(z)]
  if (is.na(offset)) 0 else unname(offset) * 3600
}
