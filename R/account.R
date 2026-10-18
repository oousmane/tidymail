#' Describe a mail account for sending and reading
#'
#' `mail_account()` holds the SMTP and IMAP settings of one login. It is the
#' shorthand for an address whose sending and reading servers share a username
#' and password. When they differ, create [smtp_account()] and [imap_account()]
#' separately. [send_mail()] and [fetch_mail()] accept a `mail_account` or the
#' account of their own transport.
#'
#' Creating an account does not read or write a password, and the object holds
#' none. Save a password with [register_creds()], which stores one entry per
#' configured transport, or set the environment variable named by
#' `pass_envvar`. The environment variable is read first, then the keychain.
#'
#' @param provider One of `"gmail"`, `"outlook"`, `"yahoo"` or `"custom"`. Sets
#'   the default hosts and ports.
#' @param email The account's email address.
#' @param ... These dots require every argument after `email` to be named.
#' @param pass_envvar Name of the environment variable that holds the password.
#'   Defaults to a name specific to the provider.
#' @param smtp_host,smtp_port,imap_host,imap_port Server settings that override
#'   the provider's. `provider = "custom"` has no default hosts: set the one you
#'   use, `smtp_host` for [send_mail()] or `imap_host` for [fetch_mail()].
#' @param use_ssl Whether to use SSL/TLS for both servers. Default `TRUE`.
#'
#' @return An object of class `mail_account`.
#'
#' @examples
#' account <- mail_account("gmail", email = "me@gmail.com")
#' account
#'
#' @export
mail_account <- function(provider = c("gmail", "outlook", "yahoo", "custom"),
                         email,
                         ...,
                         pass_envvar = NULL,
                         smtp_host = NULL,
                         smtp_port = NULL,
                         imap_host = NULL,
                         imap_port = NULL,
                         use_ssl = TRUE) {
  reject_extra_args("mail_account", ...)
  provider <- match.arg(provider)

  if (missing(email) || length(email) != 1L || !isTRUE(is_address(email))) {
    stop("`email` must be a single valid email address.", call. = FALSE)
  }

  defaults <- .providers[[provider]]

  # Each host is optional (only the one in use is needed), but a given host
  # must be a non-blank name.
  if (!is.null(smtp_host) && !is_nonblank_string(smtp_host)) {
    stop("`smtp_host` must be a non-empty hostname, e.g. \"smtp.example.com\".", call. = FALSE)
  }
  if (!is.null(imap_host) && !is_nonblank_string(imap_host)) {
    stop("`imap_host` must be a non-empty hostname, e.g. \"imap.example.com\".", call. = FALSE)
  }
  if (!is.null(smtp_host)) smtp_host <- trimws(smtp_host)
  if (!is.null(imap_host)) imap_host <- trimws(imap_host)

  pass_envvar <- pass_envvar %||% defaults$pass_envvar
  smtp_port <- smtp_port %||% defaults$smtp_port
  imap_port <- imap_port %||% defaults$imap_port
  validate_connection(smtp_port, use_ssl, pass_envvar)
  validate_connection(imap_port, use_ssl, pass_envvar)

  structure(
    list(
      provider    = provider,
      email       = email,
      pass_envvar = pass_envvar,
      smtp_host   = smtp_host %||% defaults$smtp_host,
      smtp_port   = smtp_port,
      imap_host   = imap_host %||% defaults$imap_host,
      imap_port   = imap_port,
      use_ssl     = use_ssl
    ),
    class = "mail_account"
  )
}

#' @export
print.mail_account <- function(x, ...) {
  cat("<mail_account> ", x$email, " (", x$provider, ")\n", sep = "")
  cat("  smtp: ", if (is.null(x$smtp_host)) "(not set)" else paste0(x$smtp_host, ":", x$smtp_port), "\n", sep = "")
  cat("  imap: ", if (is.null(x$imap_host)) "(not set)" else paste0(x$imap_host, ":", x$imap_port), "\n", sep = "")
  cat(
    "  password: ",
    paste0("(from $", x$pass_envvar, " or system keychain)"),
    "\n",
    sep = ""
  )
  invisible(x)
}

#' Describe an SMTP account for sending mail
#'
#' `smtp_account()` holds the login of the server [send_mail()] delivers
#' through. It is separate from [imap_account()] because sending and reading are
#' two logins: the username, password, host and provider can differ.
#'
#' Creating an account does not read or write a password, and the object holds
#' none. Save a password with [register_creds()], or set the environment
#' variable named by `pass_envvar`. The environment variable is read first, then
#' the keychain.
#'
#' @param provider One of `"gmail"`, `"outlook"`, `"yahoo"` or `"custom"`. Sets
#'   the default `host` and `port`. `"custom"` has no default host, so `host` is
#'   required.
#' @param email The account's email address. [send_mail()] sends from it.
#' @param user The login name, when it differs from `email`. Defaults to
#'   `email`.
#' @param ... These dots require every argument after `user` to be named.
#' @param pass_envvar Name of the environment variable that holds the password.
#'   Defaults to `TIDYMAIL_<PROVIDER>_SMTP_PASSWORD` (`_IMAP_` for
#'   [imap_account()]).
#' @param host,port Server hostname, without a `smtp://` prefix, and port.
#' @param use_ssl Whether to use SSL/TLS. Default `TRUE`.
#'
#' @return An object of class `smtp_account`.
#'
#' @examples
#' smtp_account("gmail", email = "me@gmail.com")
#' smtp_account("custom", email = "me@example.com", host = "smtp.example.com")
#'
#' @export
smtp_account <- function(provider = c("gmail", "outlook", "yahoo", "custom"),
                         email,
                         user = NULL,
                         ...,
                         pass_envvar = NULL,
                         host = NULL,
                         port = NULL,
                         use_ssl = TRUE) {
  reject_extra_args("smtp_account", ...)
  build_transport_account(
    "smtp", match.arg(provider), email, user, pass_envvar, host, port, use_ssl
  )
}

#' Describe an IMAP account for reading mail
#'
#' `imap_account()` holds the login of the server [fetch_mail()] reads from. It
#' is separate from [smtp_account()] because reading and sending are two logins:
#' the username, password, host and provider can differ.
#'
#' Creating an account does not read or write a password, and the object holds
#' none. Save a password with [register_creds()], or set the environment
#' variable named by `pass_envvar`. The environment variable is read first, then
#' the keychain.
#'
#' @inheritParams smtp_account
#' @param host,port Server hostname, without an `imap://` or `imaps://` prefix,
#'   and port.
#'
#' @return An object of class `imap_account`.
#'
#' @examples
#' imap_account("gmail", email = "me@gmail.com")
#' imap_account("custom", email = "me@example.com", host = "imap.example.com")
#'
#' @export
imap_account <- function(provider = c("gmail", "outlook", "yahoo", "custom"),
                         email,
                         user = NULL,
                         ...,
                         pass_envvar = NULL,
                         host = NULL,
                         port = NULL,
                         use_ssl = TRUE) {
  reject_extra_args("imap_account", ...)
  build_transport_account(
    "imap", match.arg(provider), email, user, pass_envvar, host, port, use_ssl
  )
}

print_transport_account <- function(x, label) {
  cat("<", label, "> ", x$user, " @ ", x$host, ":", x$port, " (", x$provider, ")\n", sep = "")
  cat(
    "  password: ",
    paste0("(from $", x$pass_envvar, " or system keychain)"),
    "\n",
    sep = ""
  )
  invisible(x)
}

#' @export
print.smtp_account <- function(x, ...) print_transport_account(x, "smtp_account")

#' @export
print.imap_account <- function(x, ...) print_transport_account(x, "imap_account")

# The single-transport account that send_mail() or fetch_mail() needs, taken
# from that transport's own account or from a mail_account().
as_transport_account <- function(account, transport) {
  cls <- paste0(transport, "_account")
  if (inherits(account, cls)) {
    return(account)
  }
  if (!inherits(account, "mail_account")) {
    stop(
      "`account` must be an `", cls, "` object, as returned by ", cls, "() ",
      "(or a `mail_account` from mail_account()).",
      call. = FALSE
    )
  }
  host <- account[[paste0(transport, "_host")]]
  if (is.null(host)) {
    stop(
      "`account` has no `", transport, "_host` set. Use ", cls, "(..., host = ), ",
      "or mail_account(..., ", transport, "_host = ), or a provider (\"gmail\", ",
      "\"outlook\", \"yahoo\") that has one built in.",
      call. = FALSE
    )
  }
  structure(
    list(
      provider = account$provider, email = account$email, user = account$email,
      pass_envvar = account$pass_envvar,
      host = host, port = account[[paste0(transport, "_port")]], use_ssl = account$use_ssl
    ),
    class = cls
  )
}

as_smtp_account <- function(account) as_transport_account(account, "smtp")

as_imap_account <- function(account) as_transport_account(account, "imap")

build_transport_account <- function(transport, provider, email, user,
                                    pass_envvar, host, port, use_ssl) {
  if (missing(email) || length(email) != 1L || !isTRUE(is_address(email))) {
    stop("`email` must be a single valid email address.", call. = FALSE)
  }
  if (!is.null(user) && !is_nonblank_string(user)) {
    stop("`user` must be a non-empty string.", call. = FALSE)
  }
  if (!is.null(host) && !is_nonblank_string(host)) {
    stop("`host` must be a non-empty hostname, e.g. \"", transport, ".example.com\".", call. = FALSE)
  }

  defaults <- .providers[[provider]]
  host <- if (is.null(host)) defaults[[paste0(transport, "_host")]] else trimws(host)
  if (is.null(host)) {
    stop(
      "provider = \"custom\" requires `host`, e.g. \"", transport, ".example.com\".",
      call. = FALSE
    )
  }

  port <- port %||% defaults[[paste0(transport, "_port")]]
  pass_envvar <- pass_envvar %||% sprintf("TIDYMAIL_%s_%s_PASSWORD", toupper(provider), toupper(transport))
  validate_connection(port, use_ssl, pass_envvar)

  structure(
    list(
      provider    = provider,
      email       = email,
      user        = if (is.null(user)) email else trimws(user),
      pass_envvar = pass_envvar,
      host        = host,
      port        = port,
      use_ssl     = use_ssl
    ),
    class = paste0(transport, "_account")
  )
}

validate_connection <- function(port, use_ssl, pass_envvar) {
  if (!is.numeric(port) || length(port) != 1L || is.na(port) ||
        !is.finite(port) || port < 1 || port > 65535 || port != trunc(port)) {
    stop("`port` must be a whole number between 1 and 65535.", call. = FALSE)
  }
  validate_flag(use_ssl, "use_ssl")
  if (!is_nonblank_string(pass_envvar) || grepl("=", pass_envvar, fixed = TRUE)) {
    stop("`pass_envvar` must be a non-empty environment variable name without '='.", call. = FALSE)
  }
}

# The dots of an account constructor only make later arguments named-only, so
# anything passed through them is an error. A `password` gets a pointer to the
# keychain functions.
reject_extra_args <- function(fn, ...) {
  n <- ...length()
  if (n == 0L) {
    return(invisible())
  }
  nms <- ...names() %||% rep("", n)
  nms[is.na(nms)] <- ""
  if ("password" %in% nms) {
    stop(
      "`password` is not an argument of ", fn, "(). Save a password with ",
      "register_creds(account, password = ), or set the environment variable named by `pass_envvar`.",
      call. = FALSE
    )
  }
  if (any(!nzchar(nms))) {
    stop(
      fn, "() takes its first arguments by position and the rest by name (for example `host =`).",
      call. = FALSE
    )
  }
  stop("Unused argument", if (n > 1L) "s", ": ", paste0("`", nms, "`", collapse = ", "), ".", call. = FALSE)
}

# Default server settings of the supported providers.
.providers <- list(
  gmail = list(
    smtp_host = "smtp.gmail.com", smtp_port = 587,
    imap_host = "imap.gmail.com", imap_port = 993,
    pass_envvar = "TIDYMAIL_GMAIL_PASSWORD"
  ),
  outlook = list(
    smtp_host = "smtp.office365.com", smtp_port = 587,
    imap_host = "outlook.office365.com", imap_port = 993,
    pass_envvar = "TIDYMAIL_OUTLOOK_PASSWORD"
  ),
  yahoo = list(
    smtp_host = "smtp.mail.yahoo.com", smtp_port = 587,
    imap_host = "imap.mail.yahoo.com", imap_port = 993,
    pass_envvar = "TIDYMAIL_YAHOO_PASSWORD"
  ),
  custom = list(
    smtp_host = NULL, smtp_port = 587,
    imap_host = NULL, imap_port = 993,
    pass_envvar = "TIDYMAIL_PASSWORD"
  )
)
