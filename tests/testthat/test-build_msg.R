test_that("build_msg is the one place a message is described: fields in a fixed order", {
  expect_identical(
    names(formals(build_msg)),
    c("to", "subject", "body", "...", "cc", "bcc", "attachments", "use_md")
  )
})

test_that("build_msg describes the message; the sender is not part of it", {
  msg <- build_msg(
    c("a@example.com", "b@example.com"), "Hi", "Hello",
    cc = "c@example.com"
  )
  expect_s3_class(msg, "tidymail_msg")
  expect_length(msg$to, 2)
  expect_null(msg$from)            # the sender is the account's address, decided by send_mail()
  expect_true(msg$use_md)

  expect_error(build_msg(character(0), "x", "y"), "`to`")
  expect_error(build_msg(1, "x", "y"), "`to`")
  expect_error(build_msg("a@example.com", "x", "y", cc = 1), "cc")
})

test_that("an address that does not look right is a warning that names it and says how to inspect, not an error", {
  expect_silent(build_msg("a@example.com", "x", "y"))      # nothing to say when it is fine
  expect_warning(build_msg("not an address", "x", "y"), "`to`: not a valid address: \"not an address\".*print\\(msg\\)")
  expect_warning(build_msg(c("ok@example.com", NA), "x", "y"), "`to`.*NA")
  expect_warning(build_msg("a@example.com", "x", "y", cc = "bad"), "`cc`")
  expect_warning(build_msg("a@example.com", "x", "y", bcc = "bad"), "`bcc`")

  # the draft is still returned, with the value as given, so it can be looked at
  msg <- suppressWarnings(build_msg(c("ok@example.com", "bad"), "x", "y"))
  expect_equal(msg$to, c("ok@example.com", "bad"))
})

test_that("a control character in an address is an error, not a warning: it could inject a header", {
  expect_error(build_msg("a@example.com\nBcc: victim@example.com", "x", "y"), "`to`.*line break")
  expect_error(build_msg("a@example.com", "x", "y", cc = "b@example.com\r"), "`cc`.*line break")
})

test_that("`from` points to the account, and unnamed arguments after body are refused", {
  expect_error(build_msg("a@example.com", "x", "y", from = "me@example.com"), "`from` is not an argument.*account")
  # a fourth positional value must never become `cc` and copy someone in
  expect_error(build_msg("a@example.com", "x", "y", "me@example.com"), "must be named")
  expect_error(build_msg("a@example.com", "x", "y", colour = "red"), "Unused argument.*colour")
})

test_that("build_msg takes attachments as absolute paths, once each, and rejects non-paths", {
  f <- tempfile()
  writeLines("content", f)
  on.exit(unlink(f))

  msg <- build_msg("a@example.com", "x", "y", attachments = c(f, f))
  expect_equal(msg$attachments, as.character(fs::path_abs(f)))          # absolute, and a repeated file is kept once
  expect_equal(build_msg("a@example.com", "x", "y")$attachments, character(0))
  expect_error(build_msg("a@example.com", "x", "y", attachments = c(f, NA)), "character vector")
  expect_error(build_msg("a@example.com", "x", "y", attachments = 1), "character vector")
})

test_that("a missing file, a directory or an unreadable file is a warning that names it", {
  expect_warning(
    msg <- build_msg("a@example.com", "x", "y", attachments = c("/no/such/file", tempdir())),
    "`attachments`: not found: \"/no/such/file\".*`attachments`: a directory.*preview_mail"
  )
  expect_length(msg$attachments, 2L)                        # kept as given, so the draft can be inspected

  skip_on_os("windows")
  skip_if(Sys.info()[["user"]] == "root")
  locked <- tempfile()
  writeLines("secret", locked)
  Sys.chmod(locked, "000")
  on.exit({
    Sys.chmod(locked, "644")
    unlink(locked)
  })
  expect_warning(build_msg("a@example.com", "x", "y", attachments = locked), "not readable")
})

test_that("a big attachment total gets a note, but is not a problem that blocks sending", {
  big <- withr::local_tempfile()
  writeBin(raw(21 * 1024^2), big)
  expect_warning(msg <- build_msg("a@example.com", "x", "y", attachments = big), "add up to 21.*25 MB")
  expect_length(msg_problems(msg)$errors, 0L)
})

test_that("add_attachment appends a file, and warns about the one it was given when it is not usable", {
  msg <- build_msg("you@example.com", "x", "y")
  f <- tempfile()
  writeLines("content", f)
  on.exit(unlink(f))

  expect_silent(with_file <- add_attachment(msg, f))
  expect_equal(with_file$attachments, as.character(fs::path_abs(f)))
  expect_warning(add_attachment(with_file, "/no/such/file"), "The attachment.*not found")
  expect_warning(add_attachment(msg, tempdir()), "a directory")
  expect_error(add_attachment(msg, c(f, f)), "single file path")
  expect_error(add_attachment("not a msg", f), "tidymail_msg")
  # only the new file is reported, not the problems the draft already had
  bad <- suppressWarnings(build_msg("bad", "x", "y"))
  expect_silent(add_attachment(bad, f))
})

test_that("missing content and invalid flags are rejected", {
  expect_error(build_msg("b@example.com", NA_character_, "body"), "subject")
  expect_error(build_msg("b@example.com", "Bad\r\nInjected: header", "body"), "subject")
  expect_error(build_msg("b@example.com", "subject", NA_character_), "body")
  expect_error(build_msg("b@example.com", "subject", "body", use_md = NA), "use_md")
})

test_that("printing a draft shows its recipients and subject", {
  out <- paste(capture.output(print(build_msg("you@example.com", "Hello", "Hi"))), collapse = "\n")
  expect_match(out, "you@example.com", fixed = TRUE)
  expect_match(out, "Subject: Hello", fixed = TRUE)
  expect_no_match(out, "refuse")
})

test_that("printing a draft with problems marks each one", {
  msg <- suppressWarnings(build_msg(c("ok@example.com", "bad@"), "Hello", "Hi", attachments = "/no/such/report.pdf"))
  out <- paste(capture.output(print(msg)), collapse = "\n")
  expect_match(out, "bad@ (not a valid address)", fixed = TRUE)
  expect_no_match(out, "ok@example.com (not", fixed = TRUE)
  expect_match(out, "report.pdf (not found)", fixed = TRUE)
  expect_match(out, "send_mail() will refuse this draft: 2 problems", fixed = TRUE)
})

test_that("a linked attachment keeps the name it was given", {
  skip_on_os("windows")
  dir <- withr::local_tempdir()
  target <- file.path(dir, "report_v3_final.pdf")
  writeLines("x", target)
  link <- file.path(dir, "report.pdf")
  file.symlink(target, link)
  expect_equal(basename(build_msg("a@example.com", "x", "y", attachments = link)$attachments), "report.pdf")
})
