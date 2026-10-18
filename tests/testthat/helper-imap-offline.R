# A real mRpostman connection object that never touches the network: the only
# thing that talks to the server is curl_perform(), which these helpers replace
# with canned IMAP replies. Everything around it -- building the FETCH requests,
# cleaning the replies, parsing BODYSTRUCTURE, decoding a part, choosing file
# names and writing them -- is mRpostman's own code.
offline_imap_con <- function(folder = "INBOX", username = "me@example.com") {
  con <- mRpostman::configure_imap(
    url = "imaps://imap.example.com:993", username = username, password = "x"
  )
  con$con_params$folder <- folder   # as if select_folder() had run
  con
}

# Replies are handed out in the order mRpostman asks: one per request. Returns a
# function reporting how many requests were made.
mock_imap_replies <- function(replies, .env = parent.frame()) {
  calls <- 0L
  testthat::local_mocked_bindings(
    curl_perform = function(url, handle, self) {
      calls <<- calls + 1L
      if (calls > length(replies)) stop("unexpected extra IMAP request")
      list(headers = charToRaw(replies[[calls]]))
    },
    .package = "mRpostman", .env = .env
  )
  function() calls
}

imap_reply_structure <- function(uid, structure) {
  paste0("* 1 FETCH (UID ", uid, " ", structure, ")\r\nA1 OK FETCH completed.\r\n")
}

# `base64` is the already-encoded payload, e.g. "SGVsbG8gd29ybGQ=" for "Hello world"
imap_reply_part <- function(uid, part, base64) {
  paste0("* 1 FETCH (UID ", uid, " BODY[", part, "] {", nchar(base64), "}\r\n", base64,
         ")\r\nA2 OK FETCH completed.\r\n")
}

# Use the offline connection in place of a real login, and treat folder selection
# as already done. Pass a function to get a fresh connection for every call, as
# the real thing does: a call disconnects when it finishes.
use_offline_imap <- function(con, .env = parent.frame()) {
  testthat::local_mocked_bindings(
    imap_connect = function(account) if (is.function(con)) con() else con,
    select_folder_or_stop = function(con, folder) {
      con$con_params$folder <- folder
      invisible(folder)
    },
    .env = .env
  )
}

b64_hello <- "SGVsbG8gd29ybGQ="   # "Hello world"
b64_pdf <- "UERGLWRhdGE="         # "PDF-data"
