test_that("mail_account resolves provider defaults", {
  account <- mail_account("gmail", email = "me@gmail.com")
  expect_s3_class(account, "mail_account")
  expect_equal(account$smtp_host, "smtp.gmail.com")
  expect_equal(account$imap_host, "imap.gmail.com")
  expect_null(account$password)
})

test_that("for custom, each host is optional but must be a real hostname if given", {
  # you only need the half you're going to use; send_mail()/fetch_mail()
  # validate their own host lazily
  expect_null(mail_account("custom", email = "me@example.com")$imap_host)
  expect_equal(
    mail_account("custom", email = "me@example.com", smtp_host = " smtp.example.com ")$smtp_host,
    "smtp.example.com"
  )
  # a blank host is what produced mRpostman's cryptic "Invalid url!" live
  expect_error(mail_account("custom", email = "me@example.com", imap_host = ""), "non-empty")
  expect_error(mail_account("custom", email = "me@example.com", imap_host = "   "), "non-empty")
})

test_that("each transport resolves its own host, port and password env var", {
  s <- smtp_account("gmail", email = "me@gmail.com")
  i <- imap_account("gmail", email = "me@gmail.com")
  expect_s3_class(s, "smtp_account")
  expect_s3_class(i, "imap_account")
  expect_equal(c(s$host, s$port), c("smtp.gmail.com", "587"))
  expect_equal(c(i$host, i$port), c("imap.gmail.com", "993"))
  expect_equal(s$pass_envvar, "TIDYMAIL_GMAIL_SMTP_PASSWORD")
  expect_equal(i$pass_envvar, "TIDYMAIL_GMAIL_IMAP_PASSWORD")
})

test_that("user defaults to email but can differ per transport", {
  expect_equal(smtp_account("custom", email = "me@example.com", host = "smtp.example.com")$user, "me@example.com")
  expect_equal(imap_account("custom", email = "me@example.com", user = "me", host = "imap.example.com")$user, "me")
})

test_that("custom needs a host; blank hosts/users and bad emails are rejected; hosts are trimmed", {
  expect_error(smtp_account("custom", email = "me@example.com"), "host")
  expect_error(imap_account("custom", email = "me@example.com", host = "  "), "non-empty")
  expect_error(imap_account("gmail", email = "me@gmail.com", user = ""), "non-empty")
  expect_error(smtp_account("gmail", email = "not an address"), "email")
  expect_equal(imap_account("custom", email = "me@example.com", host = " imap.example.com ")$host, "imap.example.com")
})

test_that("creating an account never touches the keychain, and a password argument points to register_creds()", {
  touched <- FALSE
  testthat::local_mocked_bindings(
    secure_keyring = function() {
      touched <<- TRUE
      stop("the keychain must not be used")
    },
    keyring_available = function() TRUE
  )
  smtp_account("gmail", email = "me@gmail.com")
  imap_account("gmail", email = "me@gmail.com")
  mail_account("gmail", email = "me@gmail.com")
  expect_false(touched)

  expect_error(smtp_account("gmail", "me@gmail.com", password = "hunter2"), "register_creds")
  expect_error(imap_account("gmail", "me@gmail.com", password = "hunter2"), "register_creds")
  expect_error(mail_account("gmail", "me@gmail.com", password = "hunter2"), "register_creds")
  expect_false(touched)
})

test_that("arguments after the first few must be named, so a value cannot land in pass_envvar", {
  expect_error(smtp_account("gmail", "me@gmail.com", "user", "hunter2"), "by name")
  expect_error(imap_account("gmail", "me@gmail.com", "user", "hunter2"), "by name")
  expect_error(mail_account("gmail", "me@gmail.com", "hunter2"), "by name")
  expect_error(smtp_account("gmail", "me@gmail.com", colour = "red"), "Unused argument.*colour")
})

test_that("a password is never printed or kept, even after it is registered", {
  local_test_keyring()
  account <- list(
    smtp_account("gmail", email = "me@gmail.com"),
    imap_account("gmail", email = "me@gmail.com"),
    mail_account("gmail", email = "me@gmail.com")
  )
  for (cred in account) suppressMessages(register_creds(cred, password = "hunter2"))
  printed <- vapply(account, function(x) paste(capture.output(print(x)), collapse = "\n"), character(1))
  expect_false(any(grepl("hunter2", printed, fixed = TRUE)))
  expect_true(all(grepl("system keychain", printed, fixed = TRUE)))
  expect_false(any(vapply(account, function(x) "password" %in% names(x), logical(1))))
})

test_that("a combined mail_account serves either transport, erroring only for the host it lacks", {
  m <- mail_account("custom", email = "me@example.com", smtp_host = "smtp.example.com", pass_envvar = "SHARED_PW")
  s <- tidymail:::as_smtp_account(m)
  expect_s3_class(s, "smtp_account")
  expect_equal(s$host, "smtp.example.com")
  expect_equal(s$pass_envvar, "SHARED_PW")
  expect_error(tidymail:::as_imap_account(m), "imap_host")
})

test_that("send_mail and fetch_mail reject the wrong transport's account before touching the network", {
  msg <- build_msg("you@example.com", "x", "y")
  expect_error(send_mail(msg, imap_account("gmail", email = "me@gmail.com")), "smtp_account")
  expect_error(fetch_mail(smtp_account("gmail", email = "me@gmail.com")), "imap_account")
})

test_that("ports, TLS flags and environment names are validated before storage", {
  for (port in list(0, 65536, 1.5, NA_real_, "993", c(143, 993))) {
    expect_error(imap_account("gmail", "me@gmail.com", port = port), "port")
    expect_error(mail_account("gmail", "me@gmail.com", smtp_port = port), "port")
  }
  for (flag in list(NA, "TRUE", 1, logical(0))) {
    expect_error(smtp_account("gmail", "me@gmail.com", use_ssl = flag), "use_ssl")
  }
  expect_error(imap_account("gmail", "me@gmail.com", pass_envvar = ""), "pass_envvar")
})

test_that("functions that act on an account take it first, as `account`; sending takes the message first", {
  first_arg <- function(f) names(formals(f))[1]
  expect_identical(
    vapply(list(fetch_mail, register_creds, forget_creds), first_arg, character(1)),
    rep("account", 3)
  )
  expect_identical(names(formals(send_mail))[1:2], c("msg", "account"))
  expect_s3_class(smtp_account("gmail", "me@gmail.com"), "smtp_account")
  expect_s3_class(imap_account("gmail", "me@gmail.com"), "imap_account")
  expect_s3_class(mail_account("gmail", "me@gmail.com"), "mail_account")
})
