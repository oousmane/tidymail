test_that("previews render metadata and the shared composition without sending", {
  msg <- build_msg("you@example.com", "Preview <test>", "**Hello**",
                   cc = "cc@example.com", bcc = "hidden@example.com")
  file <- tempfile(fileext = ".html")
  on.exit(unlink(file))
  result <- NULL
  expect_message(result <- withVisible(preview_mail(msg, file, browse = FALSE)), "Preview saved")
  expect_identical(result$value, msg)
  expect_false(result$visible)
  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_match(html, "cc@example.com", fixed = TRUE)
  expect_match(html, "hidden@example.com", fixed = TRUE)
  expect_match(html, "Preview &lt;test&gt;", fixed = TRUE)
  expect_match(html, "&lt;strong&gt;Hello&lt;/strong&gt;", fixed = TRUE)
  expect_match(html, 'sandbox=""', fixed = TRUE)
  expect_match(html, "None", fixed = TRUE)
})

test_that("previews show attachments, support HTML, and open only when requested", {
  opened <- NULL
  testthat::local_mocked_bindings(open_mail_preview = function(path) opened <<- path)
  attachment <- tempfile(fileext = ".txt")
  file <- tempfile(fileext = ".html")
  writeLines("report", attachment)
  on.exit(unlink(c(attachment, file)))
  msg <- build_msg("you@example.com", "HTML", "<h2>Hello</h2>", use_md = FALSE) |>
    add_attachment(attachment)
  suppressMessages(preview_mail(msg, file, browse = FALSE))
  expect_null(opened)
  suppressMessages(preview_mail(msg, file, browse = TRUE))
  expect_identical(opened, normalizePath(file))
  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_match(html, basename(attachment), fixed = TRUE)
  expect_match(html, "&lt;h2&gt;Hello&lt;/h2&gt;", fixed = TRUE)
})

test_that("preview inputs are validated before composing", {
  msg <- build_msg("you@example.com", "Hello", "Hi")
  expect_error(preview_mail("not a draft"), "tidymail_msg")
  expect_error(preview_mail(msg, browse = NA), "browse")
  expect_error(preview_mail(msg, file = ""), "file")
  expect_error(preview_mail(msg, file = tempdir()), "file")
})

test_that("a draft with problems can still be previewed, and the page lists them", {
  file <- withr::local_tempfile(fileext = ".html")
  msg <- suppressWarnings(build_msg("bad@", "Hello", "Hi", attachments = "/no/such/report.pdf"))
  suppressMessages(preview_mail(msg, file, browse = FALSE))
  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_match(html, "Problems", fixed = TRUE)
  expect_match(html, "not a valid address", fixed = TRUE)
  expect_match(html, "not found", fixed = TRUE)
})
