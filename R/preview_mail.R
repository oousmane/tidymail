#' Preview a message without sending it
#'
#' `preview_mail()` renders a message from [build_msg()] as a local HTML page
#' with its recipients, subject, attachment names and composed body. It uses the
#' same composition as [send_mail()] and needs no account. Mail clients can
#' render the message differently. A message with problems (see [build_msg()])
#' can be previewed too: they are listed at the top of the page.
#'
#' @param msg A message from [build_msg()].
#' @param file Path of the HTML file to write. Defaults to a temporary file. The
#'   file contains the message text and the recipient addresses.
#' @param browse Whether to open the preview in the RStudio Viewer or the
#'   browser. Defaults to `interactive()`.
#'
#' @return `msg`, invisibly, so previewing fits in a pipe. The location of the
#'   file is reported in a message.
#'
#' @examples
#' msg <- build_msg("you@example.com", "Hello", "**Hi!**")
#' preview <- tempfile(fileext = ".html")
#' msg |> preview_mail(file = preview, browse = FALSE)
#'
#' @export
preview_mail <- function(msg, file = NULL, browse = interactive()) {
  if (!inherits(msg, "tidymail_msg")) {
    stop("`msg` must be a `tidymail_msg` object, as returned by build_msg().", call. = FALSE)
  }
  validate_flag(browse, "browse")
  file <- file %||% tempfile("tidymail-preview-", fileext = ".html")
  if (!is_nonblank_string(file) || dir.exists(file)) {
    stop("`file` must be a single HTML file path.", call. = FALSE)
  }
  # Files that cannot be attached are left out of the composition and listed
  # under "Problems", so a message with problems can still be previewed.
  shown <- msg
  shown$attachments <- msg$attachments[is.na(attachment_issues(msg$attachments))]
  email <- compose_smtp_message(shown)
  htmltools::save_html(mail_preview_page(msg, email), file = file)
  path <- normalizePath(file, mustWork = TRUE)
  message("Preview saved to: ", path)
  if (browse) open_mail_preview(path)
  invisible(msg)
}

mail_preview_page <- function(msg, email) {
  tags <- htmltools::tags
  problems <- msg_problems(msg)
  field <- function(label, value) {
    if (length(value) == 0L) return(NULL)
    tags$tr(tags$th(scope = "row", label), tags$td(paste(value, collapse = ", ")))
  }
  tags$html(
    tags$head(
      tags$meta(charset = "utf-8"), tags$title(paste("Mail preview:", msg$subject)),
      tags$style(htmltools::HTML(paste0(
        "body{font-family:system-ui,sans-serif;margin:24px;color:#222}",
        "table{border-collapse:collapse;margin-bottom:24px;width:100%}",
        "th{text-align:left;vertical-align:top;padding:6px 16px 6px 0;width:100px}",
        "td{padding:6px 0;overflow-wrap:anywhere}",
        "iframe{width:100%;height:75vh;border:1px solid #ddd}"
      )))
    ),
    tags$body(
      tags$h1("Mail preview"),
      tags$table(
        field("Problems", problems$errors), field("To", msg$to), field("Cc", msg$cc),
        field("Bcc", msg$bcc), field("Subject", msg$subject),
        field("Attachments", if (length(msg$attachments)) basename(msg$attachments) else "None")
      ),
      tags$iframe(title = "Composed message body", sandbox = "",
                  srcdoc = blastula::get_html_str(email))
    )
  )
}

open_mail_preview <- function(path) {
  viewer <- getOption("viewer")
  if (is.function(viewer)) viewer(path) else utils::browseURL(path)
}
