#' Build an email message
#'
#' `build_msg()` creates a message as plain R data, independent of any
#' transport. Building and sending are separate steps: a `tidymail_msg` can be
#' printed, previewed with [preview_mail()], extended with [add_attachment()]
#' and sent with [send_mail()].
#'
#' A message has no sender. [send_mail()] sends it from the email address of the
#' account it is given, so one message can go out through any [smtp_account()].
#'
#' An address that fails [is_address()], or an attachment that is not a
#' readable file, produces a warning that names it. The message is still
#' returned, `print()` and [preview_mail()] mark the problems, and
#' [send_mail()] refuses the message until they are fixed. A line break or other
#' control character in an address is an error, because it could inject mail
#' headers. Attachments that add up to more than 20 MB produce a note, since
#' many servers refuse messages over 25 MB.
#'
#' @param to Recipient addresses, as plain addresses such as
#'   `you@example.com` (not `Name <address>`).
#' @param subject The subject, a single line.
#' @param body The body: Markdown when `use_md = TRUE`, HTML otherwise.
#' @param ... These dots require every argument after `body` to be named.
#' @param cc,bcc Addresses to copy and to blind-copy.
#' @param attachments Paths of local files to attach, as with
#'   [add_attachment()]. Paths are made absolute, and a file attached twice is
#'   kept once.
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
build_msg <- function(to, subject, body, ..., cc = NULL, bcc = NULL,
                      attachments = NULL, use_md = TRUE) {
  reject_build_args(...)
  if (!is.character(to) || length(to) == 0L) {
    stop("`to` must be a character vector with at least one email address.", call. = FALSE)
  }
  for (field in list(cc = cc, bcc = bcc)) {
    if (!is.null(field) && !is.character(field)) {
      stop("`cc` and `bcc` must be character vectors of email addresses.", call. = FALSE)
    }
  }
  if (length(subject) != 1L || !is.character(subject) || is.na(subject) || grepl("[\r\n]", subject)) {
    stop("`subject` must be a single string.", call. = FALSE)
  }
  if (length(body) != 1L || !is.character(body) || is.na(body)) {
    stop("`body` must be a single string.", call. = FALSE)
  }
  validate_flag(use_md, "use_md")
  if (!is.null(attachments) && (!is.character(attachments) || anyNA(attachments))) {
    stop("`attachments` must be a character vector of file paths.", call. = FALSE)
  }

  msg <- structure(
    list(
      to = to,
      cc = cc,
      bcc = bcc,
      subject = subject,
      body = body,
      use_md = use_md,
      attachments = character(0)
    ),
    class = "tidymail_msg"
  )
  reject_control_characters(msg)
  for (path in attachments) msg <- append_attachment(msg, path)
  warn_about_problems(msg_problems(msg))
  msg
}

# The dots make everything after `body` named-only, so a stray positional value
# can never become `cc`. A `from` gets a pointer to where the sender comes from.
reject_build_args <- function(...) {
  n <- ...length()
  if (n == 0L) {
    return(invisible())
  }
  nms <- ...names() %||% rep("", n)
  nms[is.na(nms)] <- ""
  if ("from" %in% nms) {
    stop(
      "`from` is not an argument of build_msg(): a message is sent from the email address ",
      "of the account given to send_mail(). To send as another address, use an account for it.",
      call. = FALSE
    )
  }
  if (any(!nzchar(nms))) {
    stop(
      "build_msg() takes `to`, `subject` and `body` by position; `cc`, `bcc`, `attachments` and ",
      "`use_md` must be named.",
      call. = FALSE
    )
  }
  stop("Unused argument", if (n > 1L) "s", ": ", paste0("`", nms, "`", collapse = ", "), ".", call. = FALSE)
}

#' Attach a file to a message
#'
#' `add_attachment()` adds a local file to a message from [build_msg()] and
#' returns the message, so it fits in a pipe. Nothing is sent. As in
#' [build_msg()], a path that is not a readable file produces a warning, and
#' [send_mail()] refuses the message until it is fixed.
#'
#' @param msg A `tidymail_msg`, as returned by [build_msg()].
#' @param path Path to the file.
#'
#' @return `msg`, with `path` added to its attachments.
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
    stop("`msg` must be a `tidymail_msg` object, as returned by build_msg().", call. = FALSE)
  }
  if (!is_nonblank_string(path)) {
    stop("`path` must be a single file path.", call. = FALSE)
  }
  msg <- append_attachment(msg, path)
  # Only the new file is checked; the rest was checked when the message was built.
  added <- utils::tail(msg$attachments, 1L)
  problems <- msg_problems(list(attachments = added))
  warn_about_problems(problems, what = "The attachment")
  msg
}

# Adds the absolute path whether or not the file exists, so a message built from
# another working directory still points to the same place. Links are not
# resolved: the attachment keeps the name of the path it was given.
append_attachment <- function(msg, path) {
  path <- fs::path_norm(fs::path_abs(fs::path_expand(path)))
  msg$attachments <- unique(c(msg$attachments, as.character(path)))
  msg
}

#' @export
print.tidymail_msg <- function(x, ...) {
  mark <- function(values) {
    paste0(values, ifelse(values %in% invalid_values(values), " (not a valid address)", ""))
  }
  issue <- attachment_issues(x$attachments)
  cat("<tidymail message>\n")
  cat("  To:      ", paste(mark(x$to), collapse = ", "), "\n", sep = "")
  if (length(x$cc)) cat("  Cc:      ", paste(mark(x$cc), collapse = ", "), "\n", sep = "")
  if (length(x$bcc)) cat("  Bcc:     ", paste(mark(x$bcc), collapse = ", "), "\n", sep = "")
  cat("  Subject: ", x$subject, "\n", sep = "")
  if (length(x$attachments)) {
    cat(
      "  Attachments: ",
      paste0(basename(x$attachments), ifelse(is.na(issue), "", paste0(" (", issue, ")")), collapse = ", "),
      "\n", sep = ""
    )
  }
  errors <- msg_problems(x)$errors
  if (length(errors) > 0L) {
    cat("  ! send_mail() will refuse this draft: ", length(errors), " problem",
        if (length(errors) > 1L) "s", " (see above).\n", sep = "")
  }
  invisible(x)
}

# Problems of a message, computed from its fields each time so they are never
# out of date. `errors` stop send_mail(); `notes` are informational.
msg_problems <- function(msg) {
  errors <- character(0)
  for (field in c("to", "cc", "bcc")) {
    bad <- invalid_values(msg[[field]])
    if (length(bad) > 0L) {
      errors <- c(errors, paste0(
        "`", field, "`: not a valid address: ", quote_values(bad)
      ))
    }
  }
  issue <- attachment_issues(msg$attachments)
  for (why in unique(issue[!is.na(issue)])) {
    files <- msg$attachments[which(issue == why)]
    errors <- c(errors, paste0("`attachments`: ", why, ": ", quote_values(files)))
  }

  notes <- character(0)
  usable <- msg$attachments[is.na(issue)]
  total <- sum(as.numeric(fs::file_size(usable)))
  if (total > large_attachments) {
    notes <- sprintf(
      "The attachments add up to %s; many mail servers refuse messages over 25 MB.",
      format_bytes(total)
    )
  }
  list(errors = errors, notes = notes)
}

# Why each path cannot be attached, or NA when it can.
attachment_issues <- function(paths) {
  paths <- as.character(paths)
  issue <- rep(NA_character_, length(paths))
  file <- fs::is_file(paths) # follows links; a missing file or a dangling link is FALSE
  issue[!file] <- ifelse(fs::is_dir(paths[!file]), "a directory", "not found")
  issue[file & !fs::file_access(paths, "read")] <- "not readable"
  unname(issue)
}

invalid_values <- function(x) {
  if (is.null(x)) {
    return(character(0))
  }
  x[!(is_address(x) %in% TRUE)]
}

quote_values <- function(x, max = 5L) {
  shown <- ifelse(is.na(x), "NA", paste0("\"", x, "\""))
  more <- length(shown) - max
  shown <- utils::head(shown, max)
  paste0(paste(shown, collapse = ", "), if (more > 0L) paste0(" and ", more, " more"))
}

large_attachments <- 20 * 1024^2

# A control character in a recipient address could inject mail headers, so it
# is always an error, never a warning.
reject_control_characters <- function(msg) {
  for (field in c("to", "cc", "bcc")) {
    value <- msg[[field]]
    if (any(grepl("[[:cntrl:]]", value))) {
      stop("`", field, "` contains a line break or control character.", call. = FALSE)
    }
  }
  invisible(msg)
}

warn_about_problems <- function(problems, what = "The draft") {
  lines <- c(problems$errors, problems$notes)
  if (length(lines) == 0L) {
    return(invisible())
  }
  blocking <- length(problems$errors) > 0L
  warning(
    what, if (blocking) " has problems" else " has a note", ":\n",
    paste0("* ", lines, collapse = "\n"), "\n",
    "Inspect it with print(msg) or preview_mail(msg).",
    if (blocking) " send_mail() will refuse it until it is rebuilt without them.",
    call. = FALSE
  )
}

# send_mail() refuses a message with errors. The check runs again at send time
# because files can change after the message was built.
check_sendable <- function(msg) {
  reject_control_characters(msg)
  errors <- msg_problems(msg)$errors
  if (length(errors) > 0L) {
    stop(
      "The message cannot be sent:\n", paste0("* ", errors, collapse = "\n"), "\n",
      "Inspect it with print(msg) or preview_mail(msg), then rebuild it with build_msg().",
      call. = FALSE
    )
  }
  invisible(msg)
}
