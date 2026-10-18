# Readable text from a raw MIME body: walks the multipart structure by boundary,
# decodes each part's transfer encoding and charset, prefers text/plain and
# falls back to text/html with its tags removed. Anything it cannot read yields
# the raw body.
decode_body <- function(content_type, transfer_encoding, body) {
  if (is.na(body) || !nzchar(body)) {
    return(body)
  }
  parts <- tryCatch(
    collect_text_parts(content_type, transfer_encoding, NA_character_, body),
    error = function(e) list()
  )

  plain <- Filter(function(p) identical(p$type, "text/plain") && nzchar(trimws(p$text)), parts)
  if (length(plain) > 0L) {
    return(trimws(plain[[1L]]$text))
  }
  html <- Filter(function(p) identical(p$type, "text/html") && nzchar(trimws(p$text)), parts)
  if (length(html) > 0L) {
    text <- tryCatch(html_to_text(html[[1L]]$text), error = function(e) body)
    return(if (is.na(text)) body else text)
  }
  body
}

collect_text_parts <- function(content_type, transfer_encoding, disposition, body) {
  ct <- parse_content_type(content_type)
  is_attachment <- !is.na(disposition) && grepl("^\\s*attachment", disposition, ignore.case = TRUE)
  if (is_attachment) return(list())

  if (startsWith(ct$type, "multipart/")) {
    boundary <- ct$params$boundary
    if (is.null(boundary)) {
      return(list())
    }
    out <- list()
    for (part in split_multipart(body, boundary)) {
      sp <- split_mime_part(part)
      out <- c(out, collect_text_parts(
        extract_header_field(sp$header, "Content-Type"),
        extract_header_field(sp$header, "Content-Transfer-Encoding"),
        extract_header_field(sp$header, "Content-Disposition"),
        sp$body
      ))
    }
    return(out)
  }

  if (!startsWith(ct$type, "text/")) {
    return(list())
  }
  list(list(
    type = ct$type,
    text = decode_part_text(body, transfer_encoding, ct$params$charset)
  ))
}

decode_part_text <- function(body, transfer_encoding, charset) {
  encoding <- tolower(trimws(if (is.na(transfer_encoding)) "" else transfer_encoding))
  raw <- switch(encoding,
    "quoted-printable" = qp_decode(body, space_as_underscore = FALSE),
    "base64" = base64_decode(body),
    charToRaw(body)
  )
  bytes_to_string(raw, charset)
}

# "text/plain; charset=utf-8" to the type and named parameters. A missing header
# means text/plain (RFC 2045 sec 5.2).
parse_content_type <- function(value) {
  if (is.null(value) || is.na(value) || !nzchar(trimws(value))) {
    return(list(type = "text/plain", params = list()))
  }
  pieces <- strsplit(value, ";", fixed = TRUE)[[1L]]
  params <- list()
  for (piece in pieces[-1L]) {
    kv <- regmatches(piece, regexec("^\\s*([^=\\s]+)\\s*=\\s*(.*?)\\s*$", piece, perl = TRUE))[[1L]]
    if (length(kv) == 3L) {
      params[[tolower(kv[2L])]] <- gsub("^\"|\"$", "", kv[3L])
    }
  }
  list(type = tolower(trimws(pieces[1L])), params = params)
}

# A MIME entity is headers, a blank line, then the body.
split_mime_part <- function(part) {
  m <- regexpr("\r?\n\r?\n", part)
  if (m == -1L) {
    return(list(header = "", body = part))
  }
  list(
    header = substr(part, 1L, m - 1L),
    body = substr(part, m + attr(m, "match.length"), nchar(part))
  )
}

split_multipart <- function(body, boundary) {
  lines <- strsplit(body, "\r?\n")[[1L]]
  trimmed <- trimws(lines, which = "right")
  delim <- paste0("--", boundary)
  starts <- which(trimmed == delim)
  if (length(starts) == 0L) {
    return(character(0))
  }
  closing <- which(trimmed == paste0(delim, "--"))
  bounds <- c(starts, if (length(closing)) closing[1L] else length(lines) + 1L)

  vapply(seq_along(starts), function(i) {
    from <- bounds[i] + 1L
    to <- bounds[i + 1L] - 1L
    if (from > to) "" else paste(lines[from:to], collapse = "\n")
  }, character(1))
}

html_entities <- c(
  nbsp = 32L, amp = 38L, lt = 60L, gt = 62L, quot = 34L, apos = 39L,
  eacute = 233L, egrave = 232L, ecirc = 234L, euml = 235L, agrave = 224L,
  acirc = 226L, ccedil = 231L, icirc = 238L, iuml = 239L, ocirc = 244L,
  ugrave = 249L, ucirc = 251L, Eacute = 201L, Egrave = 200L, Agrave = 192L,
  Ccedil = 199L, laquo = 171L, raquo = 187L, lsquo = 8216L, rsquo = 8217L,
  ldquo = 8220L, rdquo = 8221L, ndash = 8211L, mdash = 8212L, hellip = 8230L,
  euro = 8364L, copy = 169L, deg = 176L
)

html_to_text <- function(html) {
  x <- gsub("(?is)<(script|style)[^>]*>.*?</\\1>", "", html, perl = TRUE)
  x <- gsub("(?i)</p>", "\n\n", x, perl = TRUE)
  x <- gsub("(?i)<br\\s*/?>|</(div|tr|li|h[1-6])>", "\n", x, perl = TRUE)
  x <- gsub("<[^>]+>", "", x)
  x <- decode_html_entities(x)
  x <- gsub("[ \t]+\n", "\n", x)
  x <- gsub("\n{3,}", "\n\n", x)
  trimws(x)
}

# Decodes entities in one left-to-right pass. Decoding "&amp;" first and then
# rescanning would turn the literal text "&amp;lt;" into "<".
decode_html_entities <- function(x) {
  m <- gregexpr("&(#[0-9]+|#[xX][0-9A-Fa-f]+|[A-Za-z]+);", x)
  found <- regmatches(x, m)[[1L]]
  if (length(found) == 0L) {
    return(x)
  }
  regmatches(x, m) <- list(vapply(found, entity_text, character(1), USE.NAMES = FALSE))
  x
}

# The character an entity stands for, or the entity itself when it is unknown or
# its code point is out of range or a surrogate.
entity_text <- function(entity) {
  name <- substr(entity, 2L, nchar(entity) - 1L)
  code <- if (grepl("^#[xX]", name)) {
    strtoi(substring(name, 3L), base = 16L)
  } else if (startsWith(name, "#")) {
    strtoi(substring(name, 2L), base = 10L)
  } else {
    unname(html_entities[name])
  }
  valid <- length(code) == 1L && !is.na(code) && code > 0L &&
    code <= 0x10ffffL && !(code >= 0xd800L && code <= 0xdfffL)
  if (valid) intToUtf8(code) else entity
}
