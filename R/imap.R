imap_connect <- function(account) {
  mRpostman::configure_imap(
    url = imap_url(account), username = account$user,
    password = resolve_password(account), use_ssl = account$use_ssl,
    buffersize = 512000
  )
}

# The "imaps://host:port" URL configure_imap() requires. A scheme already in the
# host and surrounding whitespace are removed; an empty host is an error.
imap_url <- function(account) {
  host <- trimws(account$host %||% "")
  host <- sub("^(?i)imaps?://", "", host, perl = TRUE)
  host <- sub("/+$", "", host)
  if (!nzchar(host)) {
    stop(
      "The IMAP host is empty. Set it via imap_account(..., host = ), ",
      "as a bare hostname (e.g. \"imap.example.com\", no \"imap://\" prefix).",
      call. = FALSE
    )
  }
  port <- account$port %||% if (account$use_ssl) 993 else 143
  paste0(if (account$use_ssl) "imaps://" else "imap://", host, ":", port)
}

# Selects a folder, or stops with the folders that exist on the server.
select_folder_or_stop <- function(con, folder) {
  tryCatch(
    con$select_folder(folder = folder),
    error = function(e) {
      available <- tryCatch(as.character(unlist(con$list_mail_folders()$root)), error = function(e2) character(0))
      stop(
        "Could not open folder \"", folder, "\"",
        if (length(available)) paste0(" (folders on this server: ", paste(available, collapse = ", "), ")"),
        ": ", conditionMessage(e),
        call. = FALSE
      )
    }
  )
  invisible(folder)
}

# The messages of one folder as a tibble.
fetch_folder <- function(con, folder, request, include_body, n_max = Inf, body_limit = 262144) {
  select_folder_or_stop(con, folder)

  ids <- con$search(request = request)
  if (length(ids) == 0L) {
    return(empty_mails())
  }
  # UIDs only grow, so the highest are the newest; servers don't promise an order
  ids <- sort(ids)
  if (length(ids) > n_max) {
    message(
      "Fetching the newest ", format(n_max), " of ", length(ids),
      " matching messages in \"", folder, "\"."
    )
    ids <- utils::tail(ids, n_max)
  }

  envelopes <- con$fetch_envelope(ids)
  id_col <- envelopes$id %||% envelopes$uid %||% ids
  keys <- id_string(id_col)
  n <- length(keys)

  headers <- fetch_decoded_headers(con, ids, keys, envelopes, include_body)
  structure <- fetch_message_structure(con, ids)
  attachments_by_id <- split_attachment_parts(structure, id_col)
  has_attachment <- if (is.null(structure)) {
    rep(NA, n)
  } else {
    vapply(attachments_by_id[keys], function(a) nrow(a) > 0L, logical(1), USE.NAMES = FALSE)
  }
  bodies <- if (include_body) {
    fetch_bodies(con, keys, headers, body_limit)
  } else {
    rep(NA_character_, n)
  }

  tibble::tibble(
    msg_id = id_col,
    folder = folder,
    date = parse_mail_date(col_or_na(envelopes, "date", n)),
    from = headers$from,
    to = headers$to,
    cc = headers$cc,
    bcc = headers$bcc,
    subject = headers$subject,
    body = bodies,
    has_attachment = has_attachment,
    attachments = unname(attachments_by_id[keys])
  )
}

# mRpostman names each element of a fetch result "<fetch_type>UID<id>", for
# example "HEADERUID10". This re-keys the result by the bare id, which is always
# the trailing digits.
key_fetch_result <- function(x) {
  if (!is.list(x) || length(names(x)) == 0L) {
    return(x)
  }
  nm <- names(x)
  names(x) <- ifelse(
    grepl("UID[0-9]+$", nm),
    sub("^.*UID([0-9]+)$", "\\1", nm),
    sub("^.*?([0-9]+)$", "\\1", nm, perl = TRUE)
  )
  x
}

col_or_na <- function(df, name, n) {
  v <- df[[name]]
  if (is.null(v)) rep(NA_character_, n) else as.character(v)
}

# Decoded address and subject headers, plus the content headers decode_body()
# needs (fetched only when bodies are wanted). mRpostman's own decoder mishandles
# an encoded word inside "Name <address>", so the raw headers are fetched and
# decoded by decode_mime_header(). A field that comes back empty falls back to
# the envelope's value.
fetch_decoded_headers <- function(con, ids, keys, envelopes, include_body) {
  n <- length(keys)
  fields <- c("From", "To", "Cc", "Bcc", "Subject", if (include_body) c("Content-Type", "Content-Transfer-Encoding"))
  raw_headers <- tryCatch(
    key_fetch_result(con$fetch_header(ids, fields = fields)),
    error = function(e) {
      warning(
        "Could not fetch the raw headers (", conditionMessage(e), "); using the server's ",
        "parsed envelope instead, which can leave accented names and subjects undecoded.",
        call. = FALSE
      )
      NULL
    }
  )
  if (!is.null(raw_headers) && !any(keys %in% names(raw_headers))) {
    warning(
      "The raw headers returned by the server could not be matched to the messages; ",
      "using the parsed envelope instead, which can leave accented names and subjects undecoded.",
      call. = FALSE
    )
  }

  raw_field <- function(field) {
    vapply(keys, function(key) {
      if (is.null(raw_headers)) NA_character_ else extract_header_field(raw_headers[[key]], field)
    }, character(1), USE.NAMES = FALSE)
  }
  decoded_field <- function(field, envelope_name) {
    fallback <- col_or_na(envelopes, envelope_name, n)
    decoded <- vapply(raw_field(field), function(raw) {
      if (is.na(raw)) NA_character_ else decode_mime_header(raw)
    }, character(1), USE.NAMES = FALSE)
    ifelse(is.na(decoded) | !nzchar(decoded), fallback, decoded)
  }

  list(
    from = decoded_field("From", "from"),
    to = decoded_field("To", "to"),
    cc = decoded_field("Cc", "cc"),
    bcc = decoded_field("Bcc", "bcc"),
    subject = decoded_field("Subject", "subject"),
    content_type = if (include_body) raw_field("Content-Type") else rep(NA_character_, n),
    transfer_encoding = if (include_body) raw_field("Content-Transfer-Encoding") else rep(NA_character_, n)
  )
}

# Decoded bodies, fetched one message per request and limited to the first
# `body_limit` bytes. A fetched text includes the attachments, so the limit
# avoids downloading large ones, and a message that fails becomes NA with a
# warning instead of aborting the others.
fetch_bodies <- function(con, keys, headers, body_limit = Inf) {
  partial <- if (is.finite(body_limit)) sprintf("0.%.0f", body_limit) # IMAP <offset.count>
  failed <- character(0)
  first_error <- NULL
  matched <- FALSE
  reached_limit <- 0L

  bodies <- vapply(seq_along(keys), function(i) {
    result <- tryCatch(
      key_fetch_result(con$fetch_text(as.numeric(keys[i]), partial = partial)),
      error = function(e) {
        failed <<- c(failed, keys[i])
        if (is.null(first_error)) first_error <<- conditionMessage(e)
        NULL
      }
    )
    b <- result[[keys[i]]]
    if (is.null(b)) {
      return(NA_character_)
    }
    matched <<- TRUE
    text <- paste(b, collapse = "\n")
    # Lines are re-joined above, so the limit test allows some slack.
    if (!is.null(partial) && nchar(text, "bytes") >= 0.9 * body_limit) {
      reached_limit <<- reached_limit + 1L
    }
    decode_body(headers$content_type[i], headers$transfer_encoding[i], text)
  }, character(1))

  if (length(failed) > 0L) {
    warning(
      "The body of ", length(failed), " message", if (length(failed) > 1L) "s",
      " could not be downloaded (UID ", paste(utils::head(failed, 5L), collapse = ", "),
      if (length(failed) > 5L) ", ...", "): ", first_error, ". Their `body` is NA.",
      call. = FALSE
    )
  } else if (!matched) {
    warning(
      "The message bodies returned by the server could not be matched to the messages; ",
      "`body` will be NA.",
      call. = FALSE
    )
  }
  if (reached_limit > 0L) {
    message(
      reached_limit, " message ", if (reached_limit == 1L) "body" else "bodies",
      " reached the ", format_bytes(body_limit), " download limit; attachments and anything ",
      "after it were not downloaded. Raise `body_limit` to read more."
    )
  }
  bodies
}

# One BODYSTRUCTURE request per message: its MIME parts with part number, type,
# size and disposition. A failure warns and returns NULL, so the caller reports
# `has_attachment` as NA instead of FALSE.
fetch_message_structure <- function(con, ids) {
  tryCatch(
    con$fetch_bodystructure(ids),
    error = function(e) {
      warning(
        "Could not read the attachment structure (", conditionMessage(e), "); ",
        "`has_attachment` is NA and `attachments` is empty for these messages.",
        call. = FALSE
      )
      NULL
    }
  )
}

# Splits the one-row-per-part table of fetch_bodystructure() into one
# attachments tibble per message, named by `id_col`. NULL, NA or an empty table
# mean no attachments.
split_attachment_parts <- function(structure, id_col) {
  keys <- id_string(id_col)
  out <- stats::setNames(replicate(length(keys), empty_attachments(), simplify = FALSE), keys)
  if (!is.data.frame(structure) || nrow(structure) == 0L) {
    return(out)
  }
  rows <- structure[is_real_attachment(structure), , drop = FALSE]
  by_message <- split(rows, id_string(rows[[1L]]))
  for (key in intersect(names(by_message), keys)) {
    r <- by_message[[key]]
    out[[key]] <- tibble::tibble(
      part = r$part,
      name = attachment_name(r$filename, r$part, r$subtype),
      content_type = paste0(r$type, "/", r$subtype),
      size = as.numeric(r$size),
      disposition = tolower(r$disposition)
    )
  }
  out
}

# The parts a mail client shows with a paperclip. mRpostman's `is_attachment` is
# also TRUE for any non-text part with a filename, such as an inline signature
# logo, so inline parts are excluded.
is_real_attachment <- function(parts) {
  disposition <- tolower(parts$disposition)
  inline <- !is.na(disposition) & disposition == "inline"
  !is.na(parts$part) & parts$is_attachment & !inline
}

# The part's filename, or mRpostman's fallback name ("part_2_1.pdf"), so a
# listing matches the files a download writes.
attachment_name <- function(filename, part, subtype) {
  fallback <- paste0("part_", gsub(".", "_", part, fixed = TRUE), ".", ifelse(is.na(subtype), "bin", subtype))
  ifelse(is.na(filename) | !nzchar(filename), fallback, filename)
}

empty_mails <- function() {
  tibble::tibble(
    msg_id = integer(0), folder = character(0), date = as.POSIXct(character(0), tz = "UTC"),
    from = character(0), to = character(0), cc = character(0),
    bcc = character(0), subject = character(0), body = character(0),
    has_attachment = logical(0), attachments = list()
  )
}

empty_attachments <- function() {
  tibble::tibble(
    part = character(0), name = character(0), content_type = character(0),
    size = numeric(0), disposition = character(0)
  )
}
