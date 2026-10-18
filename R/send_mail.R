#' Send an email
#'
#' `send_mail()` delivers a message from [build_msg()] through an SMTP account.
#' The message is described in [build_msg()]; sending needs only the message and
#' the account, so it fits in a pipe:
#'
#' ```r
#' build_msg("you@example.com", "Hello", "Hi!") |> send_mail(account)
#' ```
#'
#' The message is sent from the account's email address. A message with
#' problems (see [build_msg()]), such as an address that does not look valid or
#' an attachment that is no longer a readable file, is refused before anything
#' is sent. The password is read at send time from the environment variable of
#' the account, then from the system keychain.
#'
#' @param msg A message from [build_msg()].
#' @param account An SMTP account from [smtp_account()] or [mail_account()].
#' @param ... These dots require `verbose` to be named. Message fields such as
#'   `to` belong in [build_msg()].
#' @param verbose Whether the SMTP backend prints diagnostic output.
#'
#' @return Invisibly, the composed email that was sent.
#'
#' @examples
#' \dontrun{
#' smtp <- smtp_account("gmail", email = "me@gmail.com")
#'
#' build_msg("you@example.com", "Hello", "Hi!") |> send_mail(smtp)
#'
#' # Preview a message with an attachment before sending it
#' build_msg("you@example.com", "Report", "See attached.", attachments = "report.pdf") |>
#'   preview_mail() |>
#'   send_mail(smtp)
#' }
#'
#' @export
send_mail <- function(msg, account, ..., verbose = FALSE) {
  reject_message_fields(...)
  if (missing(msg) || !inherits(msg, "tidymail_msg")) {
    swapped <- !missing(msg) && inherits(msg, c("smtp_account", "imap_account", "mail_account"))
    stop(
      "`msg` must be a `tidymail_msg` object, as returned by build_msg().",
      if (swapped) " The message comes first: send_mail(msg, account).",
      call. = FALSE
    )
  }
  if (missing(account)) {
    stop("`account` is required: send_mail(msg, account).", call. = FALSE)
  }
  account <- as_smtp_account(account)
  validate_flag(verbose, "verbose")

  check_sendable(msg)
  msg$from <- account$email
  email <- compose_smtp_message(msg)
  smtp_deliver(email, msg, account, verbose)
  invisible(email)
}

# Message fields belong to build_msg(); passing one to send_mail() points there.
message_fields <- c("to", "subject", "body", "cc", "bcc", "attachments", "use_md")

reject_message_fields <- function(...) {
  n <- ...length()
  if (n == 0L) {
    return(invisible())
  }
  nms <- ...names() %||% rep("", n)
  nms[is.na(nms)] <- ""
  if ("from" %in% nms) {
    stop(
      "`from` is not an argument of send_mail(): a message is sent from the email address ",
      "of `account`. To send as another address, use an account for it.",
      call. = FALSE
    )
  }
  moved <- intersect(nms, message_fields)
  if (length(moved) > 0L) {
    stop(
      "send_mail() does not take message fields (", paste0("`", moved, "`", collapse = ", "),
      "). Describe the message with build_msg() and pipe it in: ",
      "build_msg(to, subject, body) |> send_mail(account).",
      call. = FALSE
    )
  }
  if (any(!nzchar(nms))) {
    stop(
      "send_mail() takes the message and the account by position; options such as ",
      "`verbose` must be named.",
      call. = FALSE
    )
  }
  stop("Unused argument", if (n > 1L) "s", ": ", paste0("`", nms, "`", collapse = ", "), ".", call. = FALSE)
}

# The blastula email for a message.
compose_smtp_message <- function(msg) {
  email <- blastula::compose_email(
    body = if (msg$use_md) blastula::md(msg$body) else htmltools::HTML(msg$body)
  )
  for (path in msg$attachments) {
    email <- blastula::add_attachment(email, file = path)
  }
  email
}

# blastula reads a password from an environment variable, so the password is
# staged in one with a unique temporary name, removed afterwards even on failure.
blastula_credentials <- function(account) {
  pass_var <- paste0("TIDYMAIL_TEMP_", Sys.getpid(), "_", basename(tempfile()))
  old <- Sys.getenv(pass_var, unset = NA_character_)
  on.exit({
    if (is.na(old)) Sys.unsetenv(pass_var)
    else do.call(Sys.setenv, stats::setNames(list(old), pass_var))
  }, add = TRUE)
  do.call(Sys.setenv, stats::setNames(list(resolve_password(account)), pass_var))
  blastula::creds_envvar(
    user = account$user, pass_envvar = pass_var, host = account$host,
    port = account$port, use_ssl = account$use_ssl
  )
}

smtp_deliver <- function(email, msg, account, verbose) {
  blastula::smtp_send(
    email = email, from = msg$from, to = msg$to, cc = msg$cc, bcc = msg$bcc,
    subject = msg$subject, credentials = blastula_credentials(account),
    verbose = verbose
  )
}
