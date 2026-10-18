#' Fetch a mailbox as a tibble
#'
#' `fetch_mail()` searches one or more IMAP folders and returns the matching
#' messages as a tibble, ready for `dplyr::filter()`, `arrange()` and the like.
#'
#' Header fields (`from`, `to`, `cc`, `bcc`, `subject`) are decoded from the raw
#' headers, so RFC 2047 encoded words come back as readable text. `body` is the
#' `text/plain` part, decoded from its transfer encoding and charset. Failing
#' that it is the `text/html` part with its tags removed, and failing that the
#' raw body.
#'
#' Attachments are listed, never downloaded. `has_attachment` is `TRUE` when a
#' message carries at least one attachment (images embedded in the message, such
#' as a signature logo, do not count), and `NA` with a warning when its
#' structure could not be read. `attachments` holds, for each message, a tibble
#' with `part`, `name`, `content_type`, `size` and `disposition`, with zero rows
#' when there is none. [attachments()] flattens it across messages and
#' [download_attachments()] saves the files.
#'
#' `msg_id` is the IMAP UID, which is unique only within a folder: `msg_id` and
#' `folder` together identify a message.
#'
#' @param account An [imap_account()] or a [mail_account()].
#' @param folder One or more IMAP folders, such as `c("INBOX", "Archives")`.
#'   Default `"INBOX"`. A folder that does not exist is an error that lists the
#'   folders on the server.
#' @param since,before Only messages sent on or after `since`, or on or before
#'   `before`. Anything `as.Date()` understands, such as a `Date` or
#'   `"2026-09-01"`.
#' @param from,subject Only messages whose `From` or `Subject` header contains
#'   this string. The server matches it against the header as stored, so `from`
#'   cannot see through a shared sender address. To find mail that involves
#'   someone sending through a shared mailbox, filter on `to` and `cc` in R.
#' @param include_body Whether to fetch the text body. Default `TRUE`. `FALSE`
#'   is much faster when only headers are needed.
#' @param n_max The most messages to fetch from each folder. When a folder has
#'   more matches, the newest ones (highest UIDs) are kept and a message reports
#'   how many were left out. Default `Inf` fetches every match, which can take a
#'   long time on a large folder: narrow the search with `since` or `from`
#'   first.
#' @param body_limit The most bytes of each message to download when
#'   `include_body = TRUE`. Default 256 KB. A message body includes its
#'   attachments, so without a limit one large attachment is downloaded in full
#'   to read a few lines of text. The text usually comes first. A message
#'   reports how many bodies reached the limit. `Inf` downloads whole messages.
#'
#' @return A tibble with one row per message, oldest first within each folder:
#'   `msg_id`, `folder`, `date` (UTC), `from`, `to`, `cc`, `bcc`, `subject`,
#'   `body`, `has_attachment` (logical) and `attachments` (a list-column of
#'   tibbles).
#'
#' @examples
#' \dontrun{
#' imap <- imap_account("custom", email = "me@example.com", host = "imap.example.com")
#' fetch_mail(imap, folder = c("INBOX", "Archives"), since = "2026-09-01")
#'
#' # The 50 newest messages in the inbox, headers only
#' fetch_mail(imap, n_max = 50, include_body = FALSE)
#' }
#'
#' @export
fetch_mail <- function(account,
                       folder = "INBOX",
                       since = NULL,
                       before = NULL,
                       from = NULL,
                       subject = NULL,
                       include_body = TRUE,
                       n_max = Inf,
                       body_limit = 262144) {
  account <- as_imap_account(account)
  if (!is.character(folder) || length(folder) == 0L || anyNA(folder) || !all(nzchar(folder))) {
    stop("`folder` must be one or more non-empty folder names, e.g. \"INBOX\".", call. = FALSE)
  }
  request <- imap_search_request(since, before, from, subject)

  validate_flag(include_body, "include_body")
  validate_count(n_max, "n_max")
  validate_count(body_limit, "body_limit")
  local_fixed_notation()
  con <- imap_connect(account)
  on.exit(con$disconnect(), add = TRUE)

  per_folder <- lapply(unique(folder), function(f) fetch_folder(con, f, request, include_body, n_max, body_limit))
  tibble::as_tibble(do.call(rbind, per_folder))
}

imap_search_request <- function(since, before, from, subject) {
  since <- search_date(since, "since")
  before <- search_date(before, "before")
  if (!is.null(since) && !is.null(before) && since > before) {
    stop("`since` must be on or before `before`.", call. = FALSE)
  }
  for (name in c("from", "subject")) {
    value <- if (name == "from") from else subject
    if (!is.null(value) && !is_nonblank_string(value)) {
      stop("`", name, "` must be a non-empty string.", call. = FALSE)
    }
  }
  if (!is.null(from)) from <- imap_quote(from, "from")
  if (!is.null(subject)) subject <- imap_quote(subject, "subject")
  criteria <- list()
  if (!is.null(since)) criteria$since <- mRpostman::sent_since(to_imap_date(since))
  if (!is.null(before)) criteria$before <- mRpostman::sent_before(to_imap_date(before + 1))
  if (!is.null(from)) criteria$from <- mRpostman::string(expr = from, where = "FROM")
  if (!is.null(subject)) criteria$subject <- mRpostman::string(expr = subject, where = "SUBJECT")

  if (length(criteria) == 0L) {
    return("ALL")
  }
  Reduce(`&`, criteria)
}

# mRpostman wraps search text in double quotes without escaping it, so a quote
# or backslash would end the string early. They are escaped as an IMAP quoted
# string requires; control characters cannot appear in one.
imap_quote <- function(x, name) {
  if (grepl("[[:cntrl:]]", x)) {
    stop("`", name, "` must not contain control characters such as line breaks.", call. = FALSE)
  }
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  gsub("\"", "\\\"", x, fixed = TRUE)
}

search_date <- function(x, name) {
  if (is.null(x)) return(NULL)
  date <- tryCatch(as.Date(x), error = function(e) as.Date(NA))
  if (length(date) != 1L || is.na(date)) {
    stop("`", name, "` must be a single valid date, e.g. \"2026-10-01\".", call. = FALSE)
  }
  date
}

# IMAP search dates are always "DD-Mon-YYYY" in English, regardless of locale.
to_imap_date <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  d <- as.Date(x)
  sprintf(
    "%02d-%s-%04d",
    as.integer(format(d, "%d")),
    month_abb_en[as.integer(format(d, "%m"))],
    as.integer(format(d, "%Y"))
  )
}

validate_count <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1 ||
        (is.finite(x) && x != trunc(x))) {
    stop("`", name, "` must be a whole number of at least 1, or Inf for no limit.", call. = FALSE)
  }
  invisible(x)
}
