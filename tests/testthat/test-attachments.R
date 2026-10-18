fake_mails <- function() {
  tibble::tibble(
    msg_id = c(1L, 2L, 3L),
    folder = c("INBOX", "INBOX", "Archives"),
    from = c("a@example.com", "b@example.com", "c@example.com"),
    has_attachment = c(TRUE, FALSE, TRUE),
    attachments = list(
      tibble::tibble(
        part = "2", name = "report.pdf", content_type = "application/pdf",
        size = 1024, disposition = "attachment"
      ),
      tibble::tibble(
        part = character(0), name = character(0), content_type = character(0),
        size = numeric(0), disposition = character(0)
      ),
      tibble::tibble(
        part = c("2", "3"), name = c("a.png", "b.png"), content_type = "image/png",
        size = c(10, 20), disposition = "attachment"
      )
    )
  )
}

test_that("attachments() flattens to one row per file, tied to msg_id and folder, and is zero-row when none", {
  emails <- fake_mails()
  out <- attachments(emails)
  expect_equal(out$msg_id, c(1L, 3L, 3L))
  expect_equal(out$folder, c("INBOX", "Archives", "Archives"))
  expect_equal(out$name, c("report.pdf", "a.png", "b.png"))
  expect_equal(out$part, c("2", "2", "3"))

  none <- attachments(emails[emails$msg_id == 2, ])
  expect_equal(nrow(none), 0L)
  expect_named(none, c("msg_id", "folder", "part", "name", "content_type", "size", "disposition"))
})

test_that("has_attachment is a column, not an exported function", {
  expect_false("has_attachment" %in% getNamespaceExports("tidymail"))
  expect_identical(fake_mails()$has_attachment, c(TRUE, FALSE, TRUE))
})

test_that("attachments() also takes the list-column on its own, and rejects anything else", {
  emails <- fake_mails()
  out <- attachments(emails$attachments)                 # no table: messages are numbered by position
  expect_equal(out$msg_id, c(1L, 3L, 3L))
  expect_false("folder" %in% names(out))
  expect_equal(out$name, c("report.pdf", "a.png", "b.png"))

  expect_error(attachments("not a table"), "fetch_mail\\(\\) tibble")
})

test_that("attachments() errors on a data frame with no attachments column", {
  expect_error(attachments(data.frame(msg_id = 1:2)), "no `attachments` column")
})

imap <- imap_account("custom", "me@example.com", host = "imap.example.com")

attachments_tbl <- function(...) {
  rows <- list(...)
  tibble::tibble(
    part = vapply(rows, `[[`, "", 1L), name = vapply(rows, `[[`, "", 2L),
    content_type = vapply(rows, `[[`, "", 3L), size = 100, disposition = "attachment"
  )
}
mail_of <- function(msg_id, attachments, folder = "INBOX") {
  tibble::tibble(msg_id = msg_id, folder = folder, attachments = attachments)
}
none <- attachments_tbl()[0, ]
pdf_row <- attachments_tbl(c("2", "report.pdf", "application/pdf"))

# A connection whose attachments() returns decoded bytes, as mRpostman does with
# dest = NULL. It requires dest = NULL: tidymail alone writes the files.
fake_downloads <- function(by_msg) {
  list(
    disconnect = function() invisible(NULL),
    attachments = function(msg_id, parts, dest, mute, ...) {
      stopifnot(is.null(dest))
      f <- by_msg[[as.character(msg_id)]]
      if (is.function(f)) return(f())
      out <- data.frame(uid = msg_id, part = f$part, filename = f$filename,
                        type = "application/pdf", size = lengths(f$bytes), stringsAsFactors = FALSE)
      out$content <- f$bytes
      out
    }
  )
}
file_of <- function(name, text, part = "2") list(part = part, filename = name, bytes = list(charToRaw(text)))
read_text <- function(path) rawToChar(readBin(path, "raw", file.size(path)))

test_that("by default files land directly in dir, named <folder>_<msg_id>_<name>", {
  dir <- withr::local_tempdir()
  use_offline_imap(offline_imap_con)
  requests <- mock_imap_replies(list(
    imap_reply_structure(10, bs_mixed(bs_text, bs_pdf)),
    imap_reply_part(10, "2", b64_pdf)
  ))
  mail <- mail_of(c(10L, 11L), list(pdf_row, none))

  out <- suppressMessages(download_attachments(mail, imap, dir))

  expect_equal(requests(), 2L)                          # message 11 has none: no request at all
  expect_named(out, c("msg_id", "folder", "part", "name", "content_type", "size", "path"))
  expect_equal(out$msg_id, 10L)
  expect_equal(out$name, "report.pdf")                  # the attachment's own name, to join back to attachments()
  expect_equal(basename(out$path), "INBOX_10_report.pdf")
  expect_equal(list.files(dir, recursive = TRUE, all.files = TRUE), "INBOX_10_report.pdf")   # no subfolders
  expect_equal(read_text(out$path), "PDF-data")          # decoded from base64
  expect_equal(out$size, 8)                              # the file's size, not the encoded size
})

test_that("simplify = FALSE keeps one folder per message and the attachment's own name", {
  dir <- withr::local_tempdir()
  use_offline_imap(fake_downloads(list(`10` = file_of("report.pdf", "x"))))
  out <- suppressMessages(download_attachments(mail_of(10L, list(pdf_row)), imap, dir, simplify = FALSE))
  expect_equal(list.files(dir, recursive = TRUE), "me@example.com/INBOX/UID10/report.pdf")
  expect_equal(out$name, "report.pdf")
})

test_that("filtering the attachments column chooses which files are downloaded", {
  dir <- withr::local_tempdir()
  use_offline_imap(offline_imap_con)
  requests <- mock_imap_replies(list(
    imap_reply_structure(10, bs_mixed(bs_text, bs_pdf, bs_zip_no_disposition)),
    imap_reply_part(10, "2", b64_pdf)
  ))
  both <- attachments_tbl(c("2", "report.pdf", "application/pdf"), c("3", "data.zip", "application/zip"))
  mail <- mail_of(10L, list(both[both$name == "report.pdf", ]))

  out <- suppressMessages(download_attachments(mail, imap, dir))
  expect_equal(requests(), 2L)                          # the structure, then only the one part
  expect_equal(out$name, "report.pdf")
})

test_that("the same file name in different messages or folders never collides", {
  dir <- withr::local_tempdir()
  use_offline_imap(fake_downloads(list(`10` = file_of("report.pdf", "first"), `11` = file_of("report.pdf", "second"))))
  mail <- rbind(
    mail_of(c(10L, 11L), rep(list(pdf_row), 2), folder = "INBOX"),
    mail_of(10L, list(pdf_row), folder = "Archives")       # same UID, another folder
  )
  out <- suppressMessages(download_attachments(mail, imap, dir))
  expect_setequal(basename(out$path), c("INBOX_10_report.pdf", "INBOX_11_report.pdf", "Archives_10_report.pdf"))
  expect_equal(read_text(file.path(dir, "INBOX_11_report.pdf")), "second")
})

test_that("running it again overwrites the same files instead of adding copies", {
  dir <- withr::local_tempdir()
  mail <- mail_of(10L, list(pdf_row))

  use_offline_imap(fake_downloads(list(`10` = file_of("report.pdf", "version 1"))))
  first <- suppressMessages(download_attachments(mail, imap, dir))
  use_offline_imap(fake_downloads(list(`10` = file_of("report.pdf", "version 2"))))
  second <- suppressMessages(download_attachments(mail, imap, dir))

  expect_equal(second$path, first$path)
  expect_equal(list.files(dir), "INBOX_10_report.pdf")      # still one file
  expect_equal(read_text(second$path), "version 2")         # the latest content
})

test_that("two attachments with the same name in one message are both kept, told apart by part", {
  dir <- withr::local_tempdir()
  use_offline_imap(fake_downloads(list(`10` = list(
    part = c("2", "3.1", "4"), filename = c("image.png", "image.png", "notes.txt"),
    bytes = list(charToRaw("a"), charToRaw("b"), charToRaw("c"))
  ))))
  rows <- attachments_tbl(c("2", "image.png", "image/png"), c("3.1", "image.png", "image/png"),
                          c("4", "notes.txt", "text/plain"))
  out <- suppressMessages(download_attachments(mail_of(10L, list(rows)), imap, dir))

  expect_equal(basename(out$path), c("INBOX_10_image-2.png", "INBOX_10_image-3_1.png", "INBOX_10_notes.txt"))
  expect_equal(vapply(out$path, read_text, "", USE.NAMES = FALSE), c("a", "b", "c"))   # nothing lost
  again <- suppressMessages(download_attachments(mail_of(10L, list(rows)), imap, dir))
  expect_equal(basename(again$path), basename(out$path))       # the same names every run
  expect_length(list.files(dir), 3L)
})

test_that("hostile filenames cannot leave dir, hide, or collide with system names", {
  safe <- tidymail:::safe_file_name
  expect_equal(safe("a/b.pdf"), "a_b.pdf")
  expect_equal(safe("C:\\x\\y.pdf"), "C__x_y.pdf")
  expect_equal(safe(".."), "_")
  expect_equal(safe(".bashrc"), "_bashrc")              # never a hidden dotfile (dir might be ~)
  expect_equal(safe("CON.txt"), "_CON.txt")             # a reserved Windows device name
  expect_equal(safe("report.pdf  "), "report.pdf")
  expect_equal(safe("a\nb\001.pdf"), "ab.pdf")          # no control characters
  expect_equal(c(safe(""), safe(NA_character_)), rep("attachment", 2))
  long <- safe(paste0(strrep("x", 400), ".pdf"))
  expect_lte(nchar(long), 180)
  expect_match(long, "\\.pdf$")                          # truncated, extension kept
  expect_equal(safe(paste0("r", intToUtf8(233L), "sum", intToUtf8(233L), ".pdf")),
               paste0("r", intToUtf8(233L), "sum", intToUtf8(233L), ".pdf"))   # accents are fine

  # a long name is cut at its end: the folder_msg_id prefix that keeps it unique stays
  prefixed <- tidymail:::flat_file_name("INBOX", 6211L, paste0(strrep("x", 400), ".pdf"))
  expect_lte(nchar(prefixed), 180)
  expect_match(prefixed, "^INBOX_6211_x")
  expect_match(prefixed, "\\.pdf$")
  # a folder called ".." cannot make the file name start with a dot or climb
  expect_equal(tidymail:::flat_file_name("..", 7L, "a.pdf"), "%2E%2E_7_a.pdf")

  # end to end, through mRpostman's own decoding: everything stays inside `parent/downloads`
  parent <- withr::local_tempdir()
  dir <- file.path(parent, "downloads")
  for (name in c("../../evil.sh", "..", "a/b.pdf", ".hidden")) {
    use_offline_imap(offline_imap_con)
    structure <- bs_mixed(bs_text, sprintf(
      '("application" "pdf" NIL NIL NIL "base64" 100 NIL ("attachment" ("filename" "%s")) NIL NIL)', name
    ))
    mock_imap_replies(list(imap_reply_structure(10, structure), imap_reply_part(10, "2", b64_pdf)))
    mail <- mail_of(10L, list(attachments_tbl(c("2", name, "application/pdf"))))
    out <- suppressMessages(download_attachments(mail, imap, dir))
    expect_equal(dirname(normalizePath(out$path)), normalizePath(dir), info = name)
  }
  expect_setequal(list.files(parent, all.files = TRUE, no.. = TRUE), "downloads")
})

test_that("a directory or a link in the way is never written through", {
  dir <- withr::local_tempdir()
  expect_silent(tidymail:::assert_writable(file.path(dir, "free.pdf")))
  writeLines("x", file.path(dir, "file.pdf"))
  expect_silent(tidymail:::assert_writable(file.path(dir, "file.pdf")))      # a plain file is replaced
  dir.create(file.path(dir, "folder.pdf"))
  expect_error(tidymail:::assert_writable(file.path(dir, "folder.pdf")), "directory or a link")

  # Sys.readlink() is NA for a path that doesn't exist and nzchar(NA) is TRUE: reading
  # that as "is a link" once made a loop spin forever.
  expect_false(tidymail:::is_link(file.path(dir, "free.pdf")))
  skip_on_os("windows")
  file.symlink(file.path(dir, "gone.pdf"), file.path(dir, "dangling.pdf"))   # points at nothing
  expect_error(tidymail:::assert_writable(file.path(dir, "dangling.pdf")), "directory or a link")
})

test_that("there is nothing to do, and nothing is contacted, when no row has an attachment", {
  testthat::local_mocked_bindings(imap_connect = function(account) stop("must not connect"))
  out <- NULL
  expect_message(out <- download_attachments(mail_of(1L, list(none)), imap, withr::local_tempdir()), "No attachments")
  expect_equal(nrow(out), 0L)
  expect_named(out, c("msg_id", "folder", "part", "name", "content_type", "size", "path"))
})

test_that("a message that cannot be downloaded warns, and the other messages still arrive", {
  dir <- withr::local_tempdir()
  use_offline_imap(fake_downloads(list(`10` = function() stop("server said no"), `11` = file_of("ok.pdf", "fine"))))
  mail <- mail_of(c(10L, 11L), list(pdf_row, pdf_row))
  expect_warning(
    out <- suppressMessages(download_attachments(mail, imap, dir)),
    "1 message.*UID 10.*server said no"
  )
  expect_equal(out$msg_id, 11L)
  expect_true(file.exists(out$path))
})

test_that("a message whose listed attachments the server no longer has is reported, not skipped", {
  gone <- data.frame(uid = integer(0), part = character(0), filename = character(0),
                     type = character(0), size = numeric(0), stringsAsFactors = FALSE)
  gone$content <- list()
  use_offline_imap(fake_downloads(list(`10` = function() gone, `11` = file_of("ok.pdf", "fine"))))
  mail <- mail_of(c(10L, 11L), list(pdf_row, pdf_row))
  expect_warning(
    out <- suppressMessages(download_attachments(mail, imap, withr::local_tempdir())),
    "UID 10.*none of the attachment parts"
  )
  expect_equal(out$msg_id, 11L)
})

test_that("when every message fails the result is an empty, correctly typed table plus a warning", {
  refuse <- function() stop("server said no")
  use_offline_imap(fake_downloads(list(`10` = refuse, `11` = refuse)))
  mail <- mail_of(c(10L, 11L), list(pdf_row, pdf_row))
  expect_warning(
    out <- suppressMessages(download_attachments(mail, imap, withr::local_tempdir())),
    "2 messages.*UID 10 in INBOX; UID 11 in INBOX"
  )
  expect_equal(nrow(out), 0L)
  expect_named(out, c("msg_id", "folder", "part", "name", "content_type", "size", "path"))
})

test_that("a path that resolves outside dir is never reported", {
  dir <- withr::local_tempdir()
  outside <- withr::local_tempfile(fileext = ".pdf")
  writeLines("x", outside)
  expect_error(tidymail:::assert_inside(outside, dir), "outside")
  inside <- file.path(dir, "ok.pdf")
  writeLines("x", inside)
  expect_silent(tidymail:::assert_inside(inside, dir))
})

test_that("bad input is rejected before any connection is made", {
  testthat::local_mocked_bindings(imap_connect = function(account) stop("must not connect"))
  d <- withr::local_tempdir()
  mail <- mail_of(10L, list(pdf_row))
  a_file <- withr::local_tempfile(fileext = ".txt")
  writeLines("x", a_file)

  expect_error(download_attachments("not a table", imap, d), "fetch_mail")
  expect_error(download_attachments(mail[, c("msg_id", "folder")], imap, d), "attachments")
  expect_error(download_attachments(mail, smtp_account("gmail", "me@gmail.com"), d), "imap_account")
  expect_error(download_attachments(mail, imap, dir = ""), "`dir`")
  expect_error(download_attachments(mail, imap, dir = a_file), "`dir`")
  expect_error(download_attachments(mail, imap, d, simplify = NA), "simplify")
  no_part_column <- mail_of(10L, list(tibble::tibble(name = "report.pdf", content_type = "application/pdf")))
  expect_error(download_attachments(no_part_column, imap, d), "`part`")
})

test_that("simplify must be named, and unknown arguments are rejected", {
  testthat::local_mocked_bindings(imap_connect = function(account) stop("must not connect"))
  d <- withr::local_tempdir()
  mail <- mail_of(10L, list(pdf_row))
  expect_error(download_attachments(mail, imap, d, TRUE), "by name")
  expect_error(download_attachments(mail, imap, d, colour = "red"), "Unused argument.*colour")
  expect_identical(names(formals(download_attachments)), c("mail", "account", "dir", "...", "simplify"))
})

test_that("folders that differ only by a character a file name cannot hold never share a prefix", {
  name <- function(folder) tidymail:::flat_file_name(folder, 5L, "a.pdf")
  expect_equal(name("INBOX"), "INBOX_5_a.pdf")
  expect_equal(name("Mail%20box"), "Mail box_5_a.pdf")          # mRpostman writes spaces as %20
  expect_equal(name("Archives/2024"), "Archives%2F2024_5_a.pdf")
  expect_equal(name("100%"), "100%25_5_a.pdf")
  folders <- c("Archives/2024", "Archives_2024", "Archives.2024", "Archives:2024", "Archives%2F2024")
  expect_length(unique(vapply(folders, name, "")), length(folders))
})

test_that("a message id such as 100000 is requested in full and named in full", {
  dir <- withr::local_tempdir()
  requests <- character(0)
  testthat::local_mocked_bindings(
    handle_setopt = function(handle, ...) {
      request <- list(...)$customrequest
      if (!is.null(request)) requests <<- c(requests, request)
      invisible(TRUE)
    },
    .package = "curl"
  )
  use_offline_imap(offline_imap_con)
  mock_imap_replies(list(
    imap_reply_structure("100000", bs_mixed(bs_text, bs_pdf)),
    imap_reply_part("100000", "2", b64_pdf)
  ))

  out <- suppressMessages(download_attachments(mail_of(100000, list(pdf_row)), imap, dir))

  expect_length(requests, 2L)
  expect_false(any(grepl("e+", requests, fixed = TRUE)))      # not "UID FETCH 1e+05"
  expect_true(all(grepl("UID FETCH 100000 ", requests, fixed = TRUE)))
  expect_equal(basename(out$path), "INBOX_100000_report.pdf")
  expect_equal(tidymail:::id_string(c(5, 100000, 2e6)), c("5", "100000", "2000000"))
})
