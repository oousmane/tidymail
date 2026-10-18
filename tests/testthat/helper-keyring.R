# Exercise password lifecycle without accessing the user's native keychain.
local_test_keyring <- function(.local_envir = parent.frame()) {
  secrets <- new.env(parent = emptyenv())
  key <- function(service, username) paste(service, username, sep = "::")
  backend <- list(
    name = "macos",
    set_with_value = function(service, username, password) {
      secrets[[key(service, username)]] <- password
    },
    get = function(service, username) {
      value <- secrets[[key(service, username)]]
      if (is.null(value)) stop("No entry")
      value
    },
    list = function(service) {
      prefix <- paste0(service, "::")
      keys <- ls(secrets)
      keys <- keys[startsWith(keys, prefix)]
      data.frame(username = substring(keys, nchar(prefix) + 1L))
    },
    delete = function(service, username) rm(list = key(service, username), envir = secrets)
  )
  testthat::local_mocked_bindings(
    secure_keyring = function() backend,
    keyring_available = function() TRUE,
    .package = "tidymail", .env = .local_envir
  )
  invisible(backend)
}
