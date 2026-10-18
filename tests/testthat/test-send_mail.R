test_that("send_mail takes a message, then an account, and says what is wrong otherwise", {
  smtp <- smtp_account("gmail", "me@gmail.com")
  msg <- build_msg("you@example.com", "x", "y")

  expect_error(send_mail("not a msg", smtp), "tidymail_msg")
  expect_error(send_mail(msg, "not an account"), "smtp_account")
  expect_error(send_mail(msg), "`account` is required")
  # account first: the message comes first
  expect_error(send_mail(smtp, msg), "send_mail\\(msg, account\\)")
  # message fields point to build_msg()
  expect_error(
    send_mail(smtp, to = "you@example.com", subject = "x", body = "y"),
    "build_msg\\(to, subject, body\\) \\|> send_mail\\(account\\)"
  )
  expect_error(send_mail(msg, smtp, from = "alias@gmail.com"), "`from` is not an argument")
  expect_error(send_mail(msg, smtp, "extra"), "by position")      # options must be named
  expect_error(send_mail(msg, smtp, verbose = NA), "verbose")
  expect_error(send_mail(msg, smtp, colour = "red"), "Unused argument.*colour")
})

test_that("send_mail names the missing smtp_host rather than handing NULL to blastula", {
  account <- mail_account("custom", email = "me@example.com", imap_host = "imap.example.com")
  msg <- build_msg("you@example.com", "x", "y")

  expect_error(send_mail(msg, account), "smtp_host")
})

test_that("a message is delivered as built, always from the account's address", {
  delivered <- list()
  testthat::local_mocked_bindings(smtp_deliver = function(email, msg, account, verbose) {
    delivered[[length(delivered) + 1L]] <<- list(email = email, msg = msg, account = account)
  })
  smtp <- smtp_account("gmail", "me@gmail.com")
  f <- tempfile(fileext = ".txt")
  writeLines("report", f)
  on.exit(unlink(f))

  msg <- build_msg(
    c("a@example.com", "b@example.com"), "Report", "**Hello**",
    cc = "c@example.com", bcc = "hidden@example.com", attachments = f
  )
  result <- withVisible(send_mail(msg, smtp))
  expect_false(result$visible)
  expect_s3_class(result$value, "email_message")
  expect_match(result$value$html_str, "<strong>Hello</strong>")
  expect_length(result$value$attachments, 1)
  expect_equal(delivered[[1]]$msg$from, "me@gmail.com")      # the account's address
  expect_equal(delivered[[1]]$msg$bcc, "hidden@example.com")
  expect_equal(delivered[[1]]$msg$attachments, as.character(fs::path_abs(f)))
  expect_null(msg$from)                                       # the draft itself is not modified

  # the same message pipes in, and goes out from whichever account it is sent with, even
  # if a sender was written into the draft by hand
  other <- smtp_account("gmail", "other@gmail.com")
  hand_edited <- build_msg("a@example.com", "HTML", "<p>HTML body</p>", use_md = FALSE)
  hand_edited$from <- "someone-else@example.com"
  hand_edited |> send_mail(other)
  expect_equal(delivered[[2]]$msg$from, "other@gmail.com")
  expect_match(delivered[[2]]$email$html_str, "<p>HTML body</p>")
})

test_that("SMTP credentials are staged briefly without leaking environment variables", {
  testthat::local_mocked_bindings(resolve_password = function(account) "test-only-secret")
  smtp <- smtp_account("gmail", "me@gmail.com")
  before <- Sys.getenv()
  backend_creds <- tidymail:::blastula_credentials(smtp)
  expect_s3_class(backend_creds, "blastula_creds")
  expect_equal(backend_creds$password, "test-only-secret")
  expect_equal(backend_creds$host, "smtp.gmail.com")
  expect_equal(Sys.getenv(), before)
})

test_that("a draft with problems is refused before anything is sent, even if it changed after being built", {
  delivered <- 0L
  testthat::local_mocked_bindings(smtp_deliver = function(...) delivered <<- delivered + 1L)
  smtp <- smtp_account("gmail", "me@gmail.com")

  bad <- suppressWarnings(build_msg(c("ok@example.com", "bad@"), "x", "y", attachments = "/no/such/file"))
  expect_error(send_mail(bad, smtp), "cannot be sent.*`to`.*bad@.*`attachments`: not found.*preview_mail")

  # a file that was fine when the draft was built, then moved or deleted
  f <- withr::local_tempfile()
  writeLines("report", f)
  msg <- build_msg("ok@example.com", "x", "y", attachments = f)
  unlink(f)
  expect_error(send_mail(msg, smtp), "not found")

  # editing the fields of a draft by hand cannot bring a header injection through
  msg <- build_msg("ok@example.com", "x", "y")
  msg$to <- "ok@example.com\nBcc: victim@example.com"
  expect_error(send_mail(msg, smtp), "line break")
  expect_equal(delivered, 0L)
})
