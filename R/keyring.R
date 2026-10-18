#' Save a password in the system keychain
#'
#' `register_creds()` stores the password of an account in the operating
#' system's credential store (macOS Keychain, Windows Credential Manager or
#' Secret Service) through the 'keyring' package. [send_mail()] and
#' [fetch_mail()] then find it without further setup. The password is neither
#' kept in the returned object nor printed.
#'
#' Entries are kept per transport and per `user@host`. Registering an
#' [smtp_account()] does not register the matching [imap_account()]; a
#' [mail_account()] registers every transport it has a host for.
#'
#' At connection time the password is read from the environment variable named
#' by the account's `pass_envvar` first, then from the keychain. Only native
#' credential stores are used: environment-variable and file backends are
#' rejected.
#'
#' `password` must be supplied, because tidymail never prompts for it. A literal
#' password can remain in scripts, console history and saved sessions. The
#' operating system may ask for permission to use its credential store.
#'
#' @param account An [smtp_account()], [imap_account()] or [mail_account()].
#' @param password The password to store, a non-empty string.
#'
#' @return `account`, invisibly.
#'
#' @examples
#' \dontrun{
#' imap <- imap_account("custom", email = "me@example.com", host = "imap.example.com")
#' register_creds(imap, password = "your-app-password")
#' fetch_mail(imap)
#'
#' forget_creds(imap)
#' }
#'
#' @export
register_creds <- function(account, password) {
  targets <- registration_targets(account)
  if (missing(password) || !is_nonblank_string(password)) {
    stop("`password` must be a non-empty string.", call. = FALSE)
  }
  backend <- secure_keyring()
  for (target in targets) {
    service <- key_service(target)
    username <- key_username(target)
    tryCatch(backend$set_with_value(service, username, password), error = function(e) {
      stop(
        "Could not save the password in the system keychain. ",
        "Check that the credential store is available and unlocked.",
        call. = FALSE
      )
    })
    message("Registered ", toupper(transport_of(target)), " login ", username, " in the system keychain.")
  }
  account$password <- NULL
  invisible(account)
}

#' Remove a saved password from the system keychain
#'
#' `forget_creds()` deletes the entries [register_creds()] created for an
#' account. Removing an entry that does not exist is not an error.
#'
#' @inheritParams register_creds
#'
#' @return `account`, invisibly.
#'
#' @export
forget_creds <- function(account) {
  targets <- registration_targets(account)
  backend <- secure_keyring()

  for (target in targets) {
    entries <- tryCatch(backend$list(service = key_service(target)), error = function(e) {
      stop("Could not inspect the system keychain. Check that it is available and unlocked.", call. = FALSE)
    })
    removed <- key_username(target) %in% entries$username
    if (removed) {
      tryCatch(backend$delete(key_service(target), key_username(target)), error = function(e) {
        stop("Could not remove the password from the system keychain.", call. = FALSE)
      })
    }
    message(
      if (removed) "Removed " else "Nothing to remove for ",
      toupper(transport_of(target)), " login ", key_username(target), "."
    )
  }
  invisible(account)
}

registration_targets <- function(account) {
  if (inherits(account, c("smtp_account", "imap_account"))) {
    return(list(account))
  }
  if (inherits(account, "mail_account")) {
    targets <- list()
    if (!is.null(account$smtp_host)) targets <- c(targets, list(as_smtp_account(account)))
    if (!is.null(account$imap_host)) targets <- c(targets, list(as_imap_account(account)))
    if (length(targets) == 0L) {
      stop("`account` has neither an `smtp_host` nor an `imap_host` to register.", call. = FALSE)
    }
    return(targets)
  }
  stop("`account` must be an `smtp_account`, `imap_account` or `mail_account` object.", call. = FALSE)
}

# One keychain entry per transport and per user@host.
key_service <- function(account) paste0("tidymail-", transport_of(account))

key_username <- function(account) paste0(account$user, "@", account$host)

transport_of <- function(account) {
  cls <- intersect(class(account), c("smtp_account", "imap_account"))
  if (length(cls) == 0L) NULL else sub("_account$", "", cls[1L])
}

keyring_available <- function() requireNamespace("keyring", quietly = TRUE)

require_keyring <- function() {
  if (!keyring_available()) {
    stop(
      "Saving a password needs the 'keyring' package: install.packages(\"keyring\").",
      call. = FALSE
    )
  }
}

secure_keyring <- function() {
  require_keyring()
  backend <- keyring::default_backend()
  if (!backend$name %in% c("macos", "wincred", "secret_service")) {
    stop(
      "A native system keychain is required. Configure macOS Keychain, Windows Credential Manager, ",
      "or Linux Secret Service; passwords will not be saved to a fallback backend.",
      call. = FALSE
    )
  }
  backend
}

# The password saved by register_creds() for `account`, or NULL. A missing
# keyring package, native keychain or entry all mean "nothing saved" and are
# silent. A native keychain that exists but cannot be read (locked, access
# denied) warns, so it is not mistaken for an unregistered account.
stored_password <- function(account) {
  if (is.null(transport_of(account)) || !keyring_available()) {
    return(NULL)
  }
  backend <- tryCatch(secure_keyring(), error = function(e) NULL)
  if (is.null(backend)) {
    return(NULL)
  }
  service <- key_service(account)
  username <- key_username(account)
  tryCatch(
    {
      entries <- backend$list(service = service)
      if (username %in% entries$username) backend$get(service, username) else NULL
    },
    error = function(e) {
      warning(
        "The system keychain could not be read (", conditionMessage(e), "), ",
        "so no saved password was used.",
        call. = FALSE
      )
      NULL
    }
  )
}

# The password to connect with: the environment variable, then the keychain.
resolve_password <- function(account) {
  pass <- Sys.getenv(account$pass_envvar, unset = NA)
  if (!is.na(pass) && nzchar(pass)) {
    return(pass)
  }
  stored <- stored_password(account)
  if (is_nonblank_string(stored)) {
    return(stored)
  }
  stop(
    "No password available. Set the ", account$pass_envvar, " environment variable, ",
    "or save the password once with register_creds(account, password = ).",
    call. = FALSE
  )
}
