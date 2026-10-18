#' Build an email message
#'
#' Constructs a message as plain R data — not a `blastula` object, not
#' anything tied to a transport. Building a message and sending it are
#' two separate steps: a `tidymail_msg` can be inspected, tested, or
#' piped through [add_attachment()] before it ever touches [send_mail()].
#'
#' The fields come in the same order as in [send_mail()]: what to say
#' (`to`, `subject`, `body`), then the optional extras. Leave `from` out and
#' the draft is sent from whichever account you send it with, so one draft can
#' go out through any of your [smtp_account()].
#'
#' @param to Recipient address(es). A character vector for several.
#' @param subject The message subject, a single line.
#' @param body The message body. Interpreted as Markdown when
#'   `use_md = TRUE` (the default), or sent as-is as HTML otherwise.
#' @param from Optional sender address. Defaults to `NULL`, meaning the
#'   address of the account is sent with.
#' @param cc,bcc Optional carbon-copy / blind-carbon-copy addresses.
#' @param attachments Optional character vector of local file paths; same as
#'   passing each to [add_attachment()].
#' @param use_md Whether `body` is Markdown. Default `TRUE`.
#'
#' @return An object of class `tidymail_msg`.
#'
#' @examples
#' msg <- build_msg(
#'   to = "you@example.com",
#'   subject = "Monthly report",
#'   body = "## Summary\n\nEverything is on track."
#' )
#' msg
#'
#' @export
build_msg <- function(
  to,
  subject,
  body,
  from = NULL,
  cc = NULL,
  bcc = NULL,
  attachments = NULL,
  use_md = TRUE
) {
  if (!is.null(from) && (length(from) != 1L || !isTRUE(is_address(from)))) {
    stop(
      "`from` must be a single valid email address, or NULL to send from the ",
      "account's own address.",
      call. = FALSE
    )
  }
  if (length(to) == 0L || !all(isTRUE_vec(is_address(to)))) {
    stop("`to` must contain only valid email addresses.", call. = FALSE)
  }
  if (!is.null(cc) && !all(isTRUE_vec(is_address(cc)))) {
    stop("`cc` must contain only valid email addresses.", call. = FALSE)
  }
  if (!is.null(bcc) && !all(isTRUE_vec(is_address(bcc)))) {
    stop("`bcc` must contain only valid email addresses.", call. = FALSE)
  }
  if (
    length(subject) != 1L ||
      !is.character(subject) ||
      is.na(subject) ||
      grepl("[\r\n]", subject)
  ) {
    stop("`subject` must be a single string.", call. = FALSE)
  }
  if (length(body) != 1L || !is.character(body) || is.na(body)) {
    stop("`body` must be a single string.", call. = FALSE)
  }
  validate_flag(use_md, "use_md")
  if (
    !is.null(attachments) && (!is.character(attachments) || anyNA(attachments))
  ) {
    stop(
      "`attachments` must be a character vector of file paths.",
      call. = FALSE
    )
  }

  msg <- structure(
    list(
      from = from,
      to = to,
      cc = cc,
      bcc = bcc,
      subject = subject,
      body = body,
      use_md = isTRUE(use_md),
      attachments = character(0)
    ),
    class = "tidymail_msg"
  )
  for (path in attachments) {
    msg <- add_attachment(msg, path)
  }
  msg
}

isTRUE_vec <- function(x) !is.na(x) & x

#' Attach a file to a message
#'
#' Adds a local file to a [build_msg()] message. Pipeable, and does not
#' send anything.
#'
#' @param msg A `tidymail_msg`, as returned by [build_msg()].
#' @param path Path to the file to attach.
#'
#' @return `msg`, with `path` appended to its attachments.
#'
#' @examples
#' report <- tempfile(fileext = ".txt")
#' writeLines("hello", report)
#'
#' msg <- build_msg(
#'   to = "you@example.com",
#'   subject = "Report",
#'   body = "See attached."
#' )
#' msg <- add_attachment(msg, report)
#'
#' @export
add_attachment <- function(msg, path) {
  if (!inherits(msg, "tidymail_msg")) {
    stop(
      "`msg` must be a `tidymail_msg` object, as returned by build_msg().",
      call. = FALSE
    )
  }
  if (!is_nonblank_string(path) || !file.exists(path) || dir.exists(path)) {
    stop("`path` must point to an existing file.", call. = FALSE)
  }
  msg$attachments <- c(msg$attachments, normalizePath(path, mustWork = TRUE))
  msg
}

#' @export
print.tidymail_msg <- function(x, ...) {
  cat("<tidymail message>\n")
  cat("  From:    ", x$from %||% "(the sending account)", "\n", sep = "")
  cat("  To:      ", paste(x$to, collapse = ", "), "\n", sep = "")
  if (length(x$cc)) {
    cat("  Cc:      ", paste(x$cc, collapse = ", "), "\n", sep = "")
  }
  if (length(x$bcc)) {
    cat("  Bcc:     ", paste(x$bcc, collapse = ", "), "\n", sep = "")
  }
  cat("  Subject: ", x$subject, "\n", sep = "")
  if (length(x$attachments)) {
    cat(
      "  Attachments: ",
      paste(basename(x$attachments), collapse = ", "),
      "\n",
      sep = ""
    )
  }
  invisible(x)
}
