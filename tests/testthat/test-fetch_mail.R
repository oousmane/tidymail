e_acute <- intToUtf8(233L)
a_grave <- intToUtf8(224L)

# --- attachments from the message structure -------------------------------

test_that("attachments come from the message structure: real attachments only, inline images excluded", {
  structure <- rbind(
    bodystructure_df(10L, bs_mixed(bs_text, bs_logo, bs_pdf)),
    bodystructure_df(11L, bs_single(bs_text)),
    bodystructure_df(12L, bs_mixed(bs_text, bs_zip_no_disposition, bs_pdf_no_name)),
    bodystructure_df(13L, bs_mixed(bs_text, bs_pdf_encoded_name))
  )
  out <- tidymail:::split_attachment_parts(structure, c(10L, 11L, 12L, 13L))

  expect_named(out, c("10", "11", "12", "13"))
  expect_named(out[["10"]], c("part", "name", "content_type", "size", "disposition"))
  expect_equal(out[["10"]]$name, "report.pdf")            # the signature logo is inline: not listed
  expect_equal(out[["10"]]$part, "3")
  expect_equal(out[["10"]]$content_type, "application/pdf")
  expect_equal(out[["10"]]$size, 4096)
  expect_equal(out[["10"]]$disposition, "attachment")
  expect_equal(nrow(out[["11"]]), 0L)                    # a single-part message has none

  # no disposition but a name counts; no name gets the name a download would write
  expect_equal(out[["12"]]$name, c("data.zip", "part_3.pdf"))
  expect_equal(out[["12"]]$part, c("2", "3"))
  expect_equal(out[["13"]]$name, paste0("r", intToUtf8(233L), "sum", intToUtf8(233L), ".pdf"))
})

test_that("a structure that isn't a data.frame means no attachments, not a crash", {
  # When nothing in the batch has attachments, mRpostman returns something
  # nrow() cannot measure.
  empty_structure <- bodystructure_df(1L, bs_single(bs_text))[0, ]
  rows <- vapply(list(NULL, list(), NA, empty_structure), function(m) {
    out <- tidymail:::split_attachment_parts(m, c(1L, 2L))
    sum(vapply(out, nrow, integer(1)))
  }, numeric(1))
  expect_equal(rows, rep(0, 4))
})

# --- key_fetch_result -------------------------------------------------------

test_that("key_fetch_result re-keys mRpostman's <type>UID<id> names by the bare id", {
  # observed in the source: results are named "HEADERUID10", not "10", so a
  # lookup by bare id silently found nothing (every body NA, every header
  # falling back to the mangled envelope)
  expect_named(
    tidymail:::key_fetch_result(list(HEADERUID10 = "a", HEADERUID7 = "b", "BODY[1]UID123" = "c")),
    c("10", "7", "123")
  )
  expect_named(tidymail:::key_fetch_result(list(TEXT7 = "a", TEXT12 = "b")), c("7", "12"))
  expect_null(tidymail:::key_fetch_result(NULL))
})

# --- fetch_folder() against a fake ImapCon ----------------------------------
# A stand-in for the mRpostman connection: only the methods fetch_folder()
# calls, returning the shapes (and result names) mRpostman really produces, so
# the whole decode/assemble path runs without a network.
fake_con <- function(folders, fetch_header = NULL, fetch_text = NULL) {
  state <- new.env()
  list(
    select_folder = function(folder) {
      if (!folder %in% names(folders)) stop("folder does not exist")
      state$folder <- folder
      invisible(NULL)
    },
    list_mail_folders = function() list(root = names(folders), children = character(0)),
    search = function(request) folders[[state$folder]]$ids,
    fetch_envelope = function(msg_id) folders[[state$folder]]$envelope,
    fetch_header = fetch_header %||% function(msg_id, fields) folders[[state$folder]]$headers,
    fetch_text = fetch_text %||% function(msg_id, partial = NULL) folders[[state$folder]]$text,
    fetch_bodystructure = function(msg_id) folders[[state$folder]]$structure
  )
}
`%||%` <- function(a, b) if (is.null(a)) b else a

one_message_folder <- function() {
  list(
    ids = 10,
    envelope = data.frame(
      id = 10L, date = "Fri, 24 Apr 2026 14:54:13 +0000",
      subject = "Qpr=C3=A9c=C3=A9dent", from = "mangled-by-mRpostman",
      to = "envelope-to@example.com", cc = NA_character_, bcc = NA_character_,
      stringsAsFactors = FALSE
    ),
    headers = list(HEADERUID10 = paste0(
      "From: =?UTF-8?Q?DABIRE_Domi=C3=A9zag?= <dabire@meteoburkina.bf>\r\n",
      "To: =?UTF-8?Q?Jos=C3=A9?= <jose@example.com>, plain@example.com\r\n",
      "Subject: =?UTF-8?Q?pr=C3=A9c=C3=A9dent?=\r\n",
      "Content-Type: text/plain; charset=UTF-8\r\n",
      "Content-Transfer-Encoding: quoted-printable\r\n\r\n"
    )),
    text = list(TEXTUID10 = "Bonjour =C3=A0 tous"),
    structure = bodystructure_df(10L, bs_mixed(bs_text, bs_pdf))
  )
}

test_that("fetch_folder assembles decoded headers, body, date and attachments", {
  con <- fake_con(list(INBOX = one_message_folder()))
  out <- tidymail:::fetch_folder(con, "INBOX", "ALL", include_body = TRUE)

  expect_equal(nrow(out), 1L)
  expect_equal(out$folder, "INBOX")
  expect_equal(out$from, paste0("DABIRE Domi", e_acute, "zag <dabire@meteoburkina.bf>"))
  expect_equal(out$to, paste0("Jos", e_acute, " <jose@example.com>, plain@example.com"))
  expect_equal(out$subject, paste0("pr", e_acute, "c", e_acute, "dent"))
  expect_equal(out$body, paste0("Bonjour ", a_grave, " tous"))
  expect_equal(out$date, as.POSIXct("2026-04-24 14:54:13", tz = "UTC"))
  expect_identical(out$has_attachment, TRUE)
  expect_equal(out$attachments[[1]]$name, "report.pdf")
  expect_null(names(out$attachments))
})

test_that("include_body = FALSE never fetches text and leaves body NA", {
  con <- fake_con(
    list(INBOX = one_message_folder()),
    fetch_text = function(msg_id, partial = NULL) stop("fetch_text must not be called")
  )
  out <- tidymail:::fetch_folder(con, "INBOX", "ALL", include_body = FALSE)
  expect_true(is.na(out$body))
})

test_that("a failing header fetch degrades to the envelope values, and says so", {
  con <- fake_con(
    list(INBOX = one_message_folder()),
    fetch_header = function(msg_id, fields) stop("server said no")
  )
  expect_warning(
    out <- tidymail:::fetch_folder(con, "INBOX", "ALL", include_body = FALSE),
    "raw headers.*server said no"
  )
  expect_equal(out$subject, "Qpr=C3=A9c=C3=A9dent")
  expect_equal(out$to, "envelope-to@example.com")
})

test_that("results that match no message warn instead of silently blanking", {
  # The fetch succeeds but its results are not matched to the messages, which
  # would otherwise fall back to the envelope without a word.
  con <- fake_con(
    list(INBOX = one_message_folder()),
    fetch_header = function(msg_id, fields) list(SOMETHINGELSE = "From: a@b.com"),
    fetch_text = function(msg_id, partial = NULL) list(SOMETHINGELSE = "body")
  )
  warned <- character(0)
  withCallingHandlers(
    tidymail:::fetch_folder(con, "INBOX", "ALL", include_body = TRUE),
    warning = function(w) {
      warned <<- c(warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_match(warned, "could not be matched to the messages", all = FALSE)
  expect_length(warned, 2L)
})

test_that("several folders bind together, an empty one included, with typed zero-row results", {
  folders <- list(INBOX = one_message_folder(), Archives = list(ids = numeric(0)), Other = one_message_folder())
  con <- fake_con(folders)
  parts <- lapply(names(folders), function(f) tidymail:::fetch_folder(con, f, "ALL", include_body = FALSE))

  expect_equal(nrow(parts[[2]]), 0L)
  expect_named(parts[[2]], names(parts[[1]]))
  # the pair that identifies a message for any later action: IMAP UID + folder
  expect_identical(names(parts[[1]])[1:2], c("msg_id", "folder"))
  expect_type(parts[[2]]$has_attachment, "logical")

  out <- tibble::as_tibble(do.call(rbind, parts))
  expect_equal(out$folder, c("INBOX", "Other"))
  expect_equal(attachments(out)$folder, c("INBOX", "Other"))
})

test_that("a nonexistent folder errors with the folders that do exist", {
  con <- fake_con(list(INBOX = one_message_folder(), Archives = list(ids = numeric(0))))
  expect_error(tidymail:::select_folder_or_stop(con, "Archive"), "folders on this server: INBOX, Archives")
})

test_that("connection adapter passes custom ports to the actual backend", {
  testthat::local_mocked_bindings(resolve_password = function(account) "test-only-secret")
  account <- imap_account("custom", "me@example.com", host = "imap.example.com", port = 1993)
  con <- tidymail:::imap_connect(account)
  expect_equal(con$con_params$url, "imaps://imap.example.com:1993")
  expect_equal(con$con_params$username, "me@example.com")
})

test_that("public fetching combines folders and always disconnects", {
  con <- fake_con(list(INBOX = one_message_folder(), Empty = list(ids = numeric(0))))
  disconnected <- FALSE
  con$disconnect <- function() disconnected <<- TRUE
  testthat::local_mocked_bindings(imap_connect = function(account) con)
  account <- imap_account("gmail", "me@gmail.com")
  out <- fetch_mail(account, folder = c("Empty", "INBOX"), include_body = FALSE)
  expect_equal(out$folder, "INBOX")
  expect_equal(out$attachments[[1]]$name, "report.pdf")
  expect_true(disconnected)
  disconnected <- FALSE
  expect_error(fetch_mail(account, folder = "Missing"), "Could not open")
  expect_true(disconnected)
  expect_error(fetch_mail(account, include_body = NA), "include_body")
})

# --- n_max ------------------------------------------------------------------

many_messages_con <- function(ids) {
  list(
    select_folder = function(folder) invisible(NULL),
    search = function(request) ids,
    fetch_envelope = function(msg_id) {
      data.frame(
        id = msg_id, date = "Fri, 24 Apr 2026 14:54:13 +0000", subject = paste0("m", msg_id),
        from = "a@example.com", to = "b@example.com", cc = NA_character_, bcc = NA_character_,
        stringsAsFactors = FALSE
      )
    },
    fetch_header = function(msg_id, fields) {
      stats::setNames(as.list(paste0("Subject: S", msg_id, "\r\n")), paste0("HEADERUID", msg_id))
    },
    fetch_text = function(msg_id, partial = NULL) stop("not needed"),
    fetch_bodystructure = function(msg_id) NULL
  )
}

test_that("n_max keeps the newest messages (highest UIDs), oldest first, and says it truncated", {
  con <- many_messages_con(c(12, 5, 9))   # servers don't promise an order
  expect_message(
    out <- tidymail:::fetch_folder(con, "INBOX", "ALL", include_body = FALSE, n_max = 2),
    "newest 2 of 3.*INBOX"
  )
  expect_equal(out$msg_id, c(9, 12))
  expect_equal(out$subject, c("S9", "S12"))

  expect_silent(all_of_them <- tidymail:::fetch_folder(con, "INBOX", "ALL", include_body = FALSE))
  expect_equal(all_of_them$msg_id, c(5, 9, 12))
})

test_that("an invalid n_max is rejected before any connection is made", {
  testthat::local_mocked_bindings(imap_connect = function(account) stop("must not connect"))
  account <- imap_account("gmail", "me@gmail.com")
  errors <- vapply(
    list(0, -1, NA, "3", c(1, 2), 1.5),
    function(v) {
      tryCatch({
        fetch_mail(account, n_max = v)
        ""
      }, error = conditionMessage)
    },
    character(1)
  )
  expect_true(all(grepl("`n_max` must be", errors, fixed = TRUE)))

  errors <- vapply(
    list(0, -1, NA, "3", c(1, 2), 1.5),
    function(v) {
      tryCatch({
        fetch_mail(account, body_limit = v)
        ""
      }, error = conditionMessage)
    },
    character(1)
  )
  expect_true(all(grepl("`body_limit` must be", errors, fixed = TRUE)))
})

# --- IMAP search text -------------------------------------------------------

test_that("search text is escaped so a quote or backslash cannot change the IMAP query", {
  quote_attack <- as.character(tidymail:::imap_search_request(NULL, NULL, 'x" OR ALL "', NULL))
  expect_equal(quote_attack, '(FROM "x\\" OR ALL \\"")')

  backslash <- as.character(tidymail:::imap_search_request(NULL, NULL, NULL, "back\\slash"))
  expect_equal(backslash, '(SUBJECT "back\\\\slash")')

  expect_error(tidymail:::imap_search_request(NULL, NULL, "line\nbreak", NULL), "control characters")
})

# --- body downloads --------------------------------------------------------
# fetch_text() returns the whole body, attachments included (base64). The text
# is requested with a byte limit, one message per request, so a large attachment
# is not downloaded in full and one failing message does not abort the fetch.

test_that("bodies are requested with a byte limit, so attachments are not downloaded in full", {
  asked <- list()
  con <- fake_con(
    list(INBOX = one_message_folder()),
    fetch_text = function(msg_id, partial = NULL) {
      asked[[length(asked) + 1L]] <<- list(id = msg_id, partial = partial)
      list(TEXTUID10 = "Bonjour =C3=A0 tous")
    }
  )
  out <- tidymail:::fetch_folder(con, "INBOX", "ALL", include_body = TRUE)
  expect_equal(out$body, paste0("Bonjour ", a_grave, " tous"))
  expect_equal(asked[[1]]$id, 10)               # one message per request
  expect_equal(asked[[1]]$partial, "0.262144")  # IMAP <offset.count>: the first 256 KB

  tidymail:::fetch_folder(con, "INBOX", "ALL", include_body = TRUE, body_limit = Inf)
  expect_null(asked[[2]]$partial)               # Inf: the whole message, as before
})

test_that("a body that cannot be downloaded is NA with a warning; the other messages still arrive", {
  con <- many_messages_con(c(5, 9))
  con$fetch_text <- function(msg_id, partial = NULL) {
    if (msg_id == 5) stop("server said no")
    stats::setNames(list("hello"), paste0("TEXTUID", msg_id))
  }
  expect_warning(
    out <- tidymail:::fetch_folder(con, "INBOX", "ALL", include_body = TRUE),
    "body of 1 message.*UID 5.*server said no"
  )
  expect_equal(out$msg_id, c(5, 9))
  expect_equal(out$body, c(NA, "hello"))
  expect_equal(out$subject, c("S5", "S9"))      # headers are unaffected
})

test_that("bodies that reached the byte limit are reported, since what came after was not read", {
  con <- many_messages_con(c(5, 9))
  con$fetch_text <- function(msg_id, partial = NULL) {
    text <- if (msg_id == 5) strrep("x", 1000) else "short"
    stats::setNames(list(text), paste0("TEXTUID", msg_id))
  }
  expect_message(
    tidymail:::fetch_folder(con, "INBOX", "ALL", include_body = TRUE, body_limit = 1000),
    "1 message body reached the.*body_limit"
  )
  expect_no_message(
    tidymail:::fetch_folder(con, "INBOX", "ALL", include_body = TRUE, body_limit = 100000)
  )

  # the default limit is reported in KB
  con$fetch_text <- function(msg_id, partial = NULL) {
    stats::setNames(list(strrep("x", 2000)), paste0("TEXTUID", msg_id))
  }
  expect_message(
    tidymail:::fetch_folder(con, "INBOX", "ALL", include_body = TRUE, body_limit = 2048),
    "reached the 2 KB download limit"
  )
})

test_that("an unreadable attachment structure warns and leaves has_attachment unknown, not FALSE", {
  con <- fake_con(list(INBOX = one_message_folder()))
  con$fetch_bodystructure <- function(msg_id) stop("server said no")
  expect_warning(
    out <- tidymail:::fetch_folder(con, "INBOX", "ALL", include_body = FALSE),
    "attachment structure.*server said no"
  )
  expect_true(is.na(out$has_attachment))        # unknown must not look like "no attachment"
  expect_equal(nrow(out$attachments[[1]]), 0L)
  expect_equal(out$subject, paste0("pr", e_acute, "c", e_acute, "dent"))   # the rest is unaffected
})

test_that("to_imap_date formats dates the way IMAP expects", {
  expect_equal(tidymail:::to_imap_date("2026-09-01"), "01-Sep-2026")
  expect_equal(tidymail:::to_imap_date(as.Date("2019-04-17")), "17-Apr-2019")
  expect_null(tidymail:::to_imap_date(NULL))
})

test_that("imap_search_request builds ALL, a single criterion, or an & combination", {
  expect_equal(tidymail:::imap_search_request(NULL, NULL, NULL, NULL), "ALL")
  expect_equal(
    as.character(tidymail:::imap_search_request("2026-09-01", NULL, NULL, NULL)),
    as.character(mRpostman::sent_since("01-Sep-2026"))
  )
  expected <- mRpostman::sent_since("01-Sep-2026") &
    mRpostman::string(expr = "a@b.com", where = "FROM") &
    mRpostman::string(expr = "hi", where = "SUBJECT")
  expect_equal(
    as.character(tidymail:::imap_search_request("2026-09-01", NULL, "a@b.com", "hi")),
    as.character(expected)
  )
})

test_that("imap_url normalises whitespace, trailing slashes and a baked-in scheme, and errors on blank", {
  url <- function(host, use_ssl = TRUE) {
    tidymail:::imap_url(structure(list(host = host, use_ssl = use_ssl), class = "imap_account"))
  }
  expect_equal(url("imap.example.com"), "imaps://imap.example.com:993")
  expect_equal(url("imap.example.com", use_ssl = FALSE), "imap://imap.example.com:143")
  expect_equal(url("  imaps://imap.example.com/ "), "imaps://imap.example.com:993")
  expect_error(url(""), "empty")
  expect_error(url(NULL), "empty")
})

test_that("search uses sent dates with both endpoints included", {
  request <- as.character(tidymail:::imap_search_request("2026-10-01", "2026-10-16", NULL, NULL))
  expect_match(request, "SENTSINCE 01-Oct-2026", fixed = TRUE)
  expect_match(request, "SENTBEFORE 17-Oct-2026", fixed = TRUE)
  expect_error(tidymail:::imap_search_request("2026-10-16", "2026-10-01", NULL, NULL), "since")
  expect_error(tidymail:::imap_search_request(NA, NULL, NULL, NULL), "valid date")
  expect_error(tidymail:::imap_search_request(c("2026-10-01", "2026-10-02"), NULL, NULL, NULL), "single")
  expect_error(tidymail:::imap_search_request(NULL, NULL, NA_character_, NULL), "from")
})

test_that("a message id such as 100000 still matches its headers and body", {
  folder <- one_message_folder()
  folder$ids <- 100000
  folder$envelope$id <- 100000
  names(folder$headers) <- "HEADERUID100000"
  names(folder$text) <- "TEXTUID100000"
  folder$structure <- bodystructure_df(100000, bs_mixed(bs_text, bs_pdf))

  out <- NULL
  expect_silent(out <- tidymail:::fetch_folder(fake_con(list(INBOX = folder)), "INBOX", "ALL", include_body = TRUE))
  expect_equal(out$msg_id, 100000)
  expect_equal(out$body, paste0("Bonjour ", intToUtf8(224L), " tous"))
  expect_true(out$has_attachment)
})
