imap_for_test <- function(...) {
  imap_account("custom", email = "me@example.com", host = "imap.example.com",
               pass_envvar = "TIDYMAIL_TEST_KR_PW", ...)
}

test_that("register_creds saves a password once and later credentials retrieve it", {
  local_test_keyring()
  withr::local_envvar(c(TIDYMAIL_TEST_KR_PW = NA))
  imap <- imap_for_test()
  expect_error(tidymail:::resolve_password(imap), "register_creds")   # nothing saved yet
  registered <- suppressMessages(register_creds(imap, password = "from-keyring"))
  expect_identical(registered, imap)             # returned unchanged; the secret is not in it
  expect_false("password" %in% names(registered))
  expect_equal(tidymail:::resolve_password(imap_for_test()), "from-keyring")
  expect_false(grepl("from-keyring", paste(capture.output(str(registered)), collapse = "")))
  Sys.setenv(TIDYMAIL_TEST_KR_PW = "from-env")
  expect_equal(tidymail:::resolve_password(imap), "from-env")
  Sys.unsetenv("TIDYMAIL_TEST_KR_PW")
  suppressMessages(forget_creds(imap))
  expect_error(tidymail:::resolve_password(imap), "register_creds")
  expect_message(forget_creds(imap), "Nothing to remove")
})

test_that("combined and independent credentials register only their transports", {
  local_test_keyring()
  withr::local_envvar(c(TIDYMAIL_TEST_KR_PW = NA))
  smtp <- smtp_account("custom", "me@example.com", host = "smtp.example.com", pass_envvar = "TIDYMAIL_TEST_KR_PW")
  suppressMessages(register_creds(smtp, password = "smtp-only"))
  expect_equal(tidymail:::resolve_password(smtp), "smtp-only")
  expect_error(tidymail:::resolve_password(imap_for_test()), "No password")
  expect_error(tidymail:::resolve_password(imap_for_test(user = "other")), "No password")
  both <- mail_account("custom", "me@example.com", smtp_host = "smtp.example.com",
                       imap_host = "imap.example.com", pass_envvar = "TIDYMAIL_TEST_KR_PW")
  said <- capture_messages(register_creds(both, password = "shared-secret"))
  expect_false(any(grepl("shared-secret", said, fixed = TRUE)))
  expect_equal(tidymail:::resolve_password(tidymail:::as_smtp_account(both)), "shared-secret")
  expect_equal(tidymail:::resolve_password(tidymail:::as_imap_account(both)), "shared-secret")
  only_smtp <- mail_account("custom", "me@example.com", smtp_host = "other.example.com")
  suppressMessages(register_creds(only_smtp, password = "single"))
  expect_equal(tidymail:::stored_password(tidymail:::as_smtp_account(only_smtp)), "single")
  expect_error(tidymail:::as_imap_account(only_smtp), "imap_host")
})

test_that("password registration never prompts or accepts invalid passwords", {
  local_test_keyring()
  imap <- imap_for_test()
  expect_error(register_creds(imap), "non-empty")
  for (password in list("", NA_character_, c("a", "b"), 123)) {
    expect_error(register_creds(imap, password), "non-empty")
  }
  expect_error(register_creds("not account", "x"), "account")
  expect_error(register_creds(mail_account("custom", "me@example.com"), "x"), "neither")
})

test_that("non-native fallback stores are rejected", {
  skip_if_not_installed("keyring")
  withr::local_options(keyring_backend = "env")
  expect_error(tidymail:::secure_keyring(), "native system keychain")
  expect_error(register_creds(imap_for_test(), "must-not-store"), "native system keychain")
  expect_null(tidymail:::stored_password(imap_for_test()))
})

test_that("missing keyring gives an actionable error", {
  testthat::local_mocked_bindings(keyring_available = function() FALSE)
  expect_error(register_creds(imap_for_test(), "x"), "install.packages")
  expect_null(tidymail:::stored_password(imap_for_test()))
})

test_that("store errors do not expose a password or pretend deletion succeeded", {
  backend <- local_test_keyring()
  backend$set_with_value <- function(...) stop("secret-in-backend-error")
  testthat::local_mocked_bindings(secure_keyring = function() backend)
  expect_error(register_creds(imap_for_test(), "secret-in-backend-error"), "^Could not save")
  backend$list <- function(...) stop("locked")
  expect_error(forget_creds(imap_for_test()), "Could not inspect")
})

test_that("a keychain that can't be read warns, while a missing entry stays silent", {
  backend <- local_test_keyring()
  imap <- imap_for_test()
  expect_silent(expect_null(tidymail:::stored_password(imap)))   # nothing saved: not an error

  backend$list <- function(...) stop("locked")
  testthat::local_mocked_bindings(secure_keyring = function() backend)
  expect_warning(expect_null(tidymail:::stored_password(imap)), "could not be read")
})

test_that("register_creds requires a password argument, with nothing to prompt for", {
  expect_identical(formals(register_creds)$password, quote(expr = ))   # required: no misleading NULL default
  expect_identical(names(formals(register_creds)), c("account", "password"))
  expect_error(register_creds(imap_for_test()), "non-empty")
})

test_that("resolve_password uses the environment without requiring a keychain", {
  testthat::local_mocked_bindings(stored_password = function(account) NULL)
  account <- mail_account("gmail", email = "me@gmail.com", pass_envvar = "TIDYMAIL_TEST_PW")
  withr::local_envvar(c(TIDYMAIL_TEST_PW = NA))
  expect_error(tidymail:::resolve_password(account), "TIDYMAIL_TEST_PW")
  Sys.setenv(TIDYMAIL_TEST_PW = "from-env")
  expect_equal(tidymail:::resolve_password(account), "from-env")
})
