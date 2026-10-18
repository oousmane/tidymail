#' List attachments across messages
#'
#' `attachments()` flattens the `attachments` column of [fetch_mail()] into one
#' row per attachment, tied to its message by `msg_id`.
#'
#' @param x The tibble from [fetch_mail()] or a subset of it, or its
#'   `attachments` column.
#'
#' @return A tibble with one row per attachment: `msg_id`, `folder` (when `x`
#'   has one; a `msg_id` is unique only within its folder), `part` (the MIME
#'   section number [download_attachments()] uses), `name`, `content_type`,
#'   `size` (bytes as transferred, so about a third more than the file for
#'   base64) and `disposition`. It has zero rows when no message has
#'   attachments.
#'
#' @examples
#' \dontrun{
#' emails |> dplyr::filter(has_attachment) |> attachments()
#' }
#'
#' @export
attachments <- function(x) {
  ids <- attachments_ids(x)
  folders <- if (is.data.frame(x) && "folder" %in% names(x)) x$folder else NULL
  col <- attachments_col(x)

  rows <- lapply(seq_along(col), function(i) {
    a <- col[[i]]
    if (!is.data.frame(a) || nrow(a) == 0L) {
      return(NULL)
    }
    head <- if (is.null(folders)) {
      tibble::tibble(msg_id = ids[i])
    } else {
      tibble::tibble(msg_id = ids[i], folder = folders[i])
    }
    tibble::as_tibble(cbind(head, a))
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]

  if (length(rows) == 0L) {
    empty <- tibble::tibble(msg_id = ids[0])
    if (!is.null(folders)) empty$folder <- character(0)
    return(tibble::as_tibble(cbind(empty, empty_attachments())))
  }

  tibble::as_tibble(do.call(rbind, rows))
}

attachments_col <- function(x) {
  if (is.data.frame(x)) {
    if (!"attachments" %in% names(x)) {
      stop("`x` is a data frame with no `attachments` column; pass the tibble from fetch_mail().", call. = FALSE)
    }
    return(x$attachments)
  }
  if (is.list(x)) {
    return(x)
  }
  stop(
    "`x` must be a fetch_mail() tibble or its `attachments` list-column.",
    call. = FALSE
  )
}

attachments_ids <- function(x) {
  if (is.data.frame(x) && "msg_id" %in% names(x)) {
    return(x$msg_id)
  }
  seq_along(attachments_col(x))
}

#' Download the attachments of fetched messages
#'
#' `download_attachments()` saves the attachments listed in the `attachments`
#' column of the rows it is given, so filtering the rows, or that column, chooses
#' the files. [fetch_mail()] only lists attachments.
#'
#' ```r
#' mail |>
#'   dplyr::filter(has_attachment) |>
#'   download_attachments(account, dir = "~/attachments")
#' ```
#'
#' By default (`simplify = TRUE`) the files are written directly into `dir`,
#' named `<folder>_<msg_id>_<file name>`, for example `INBOX_6211_report.pdf`. A
#' message is identified by its folder and `msg_id`, so names from different
#' messages never collide, and downloading again overwrites the same files. Two
#' attachments of one message with the same name each get their MIME part number
#' added (`INBOX_6211_image-2.png`, `INBOX_6211_image-3.png`), so none is lost.
#' In a folder name, each character a file name cannot hold is written as `%XX`
#' (`Archives/2024` becomes `Archives%2F2024`), so different folders keep
#' different names.
#' With `simplify = FALSE` each message gets its own folder,
#' `<dir>/<login>/<folder>/UID<msg_id>/`, and the files keep their names.
#' Existing files are overwritten in both layouts.
#'
#' Attachments come from whoever sent the message, so they are saved and never
#' opened or run. File names are made safe first: path separators and control
#' characters are replaced, a leading dot (a hidden file such as `.bashrc`) is
#' not kept, and reserved system names are avoided. A directory or a link is
#' never written through, and every path is checked to be inside `dir`.
#'
#' @param mail A table from [fetch_mail()], or any subset of its rows. Rows whose
#'   `attachments` entry is empty are skipped without contacting the server.
#' @param account The [imap_account()] (or [mail_account()]) the messages were
#'   fetched with.
#' @param dir Directory to save into, created if needed. Default: the working
#'   directory.
#' @param ... These dots require `simplify` to be named.
#' @param simplify If `TRUE` (the default), write every file directly in `dir`,
#'   named `<folder>_<msg_id>_<file name>`. If `FALSE`, keep one folder per
#'   message under `dir`.
#'
#' @return A tibble with one row per file written: `msg_id`, `folder`, `part`,
#'   `name` (the attachment's own name, as in [attachments()]), `content_type`,
#'   `size` (bytes of the saved file) and `path` (absolute, where the file was
#'   saved). It has zero rows when there was nothing to download.
#'
#' @examples
#' \dontrun{
#' imap <- imap_account("custom", email = "me@example.com", host = "imap.example.com")
#' mail <- fetch_mail(imap, since = "2026-10-01", include_body = FALSE)
#'
#' # All files in one folder: ~/attachments/INBOX_6211_report.pdf
#' download_attachments(mail[mail$has_attachment, ], imap, dir = "~/attachments")
#'
#' # One folder per message
#' download_attachments(mail, imap, "~/attachments", simplify = FALSE)
#' }
#'
#' @export
download_attachments <- function(mail, account, dir = ".", ..., simplify = TRUE) {
  reject_download_args(...)
  check_mail_table(mail)
  account <- as_imap_account(account)
  if (!is_nonblank_string(dir) || (file.exists(dir) && !dir.exists(dir))) {
    stop("`dir` must be a single directory path.", call. = FALSE)
  }
  validate_flag(simplify, "simplify")

  wanted <- which(vapply(mail$attachments, function(a) is.data.frame(a) && nrow(a) > 0L, logical(1)))
  if (length(wanted) == 0L) {
    message("No attachments to download.")
    return(empty_downloads())
  }
  local_fixed_notation()
  if (!all(vapply(mail$attachments[wanted], function(a) "part" %in% names(a), logical(1)))) {
    stop(
      "The `attachments` tables have no `part` column, so the files cannot be located. ",
      "Fetch the messages again with fetch_mail().",
      call. = FALSE
    )
  }

  con <- imap_connect(account)
  on.exit(con$disconnect(), add = TRUE)

  downloaded <- list()
  failed <- character(0)
  first_error <- NULL
  for (folder in unique(mail$folder[wanted])) {
    select_folder_or_stop(con, folder)
    for (i in wanted[mail$folder[wanted] == folder]) {
      msg_id <- mail$msg_id[i]
      written <- tryCatch(
        {
          # dest = NULL: mRpostman returns the decoded bytes; tidymail alone
          # decides file names and folders.
          result <- con$attachments(
            as.numeric(msg_id), parts = as.character(mail$attachments[[i]]$part),
            dest = NULL, mute = TRUE
          )
          save_attachments(result, msg_id, folder, dir, account, simplify)
        },
        error = function(e) {
          failed <<- c(failed, paste0("UID ", id_string(msg_id), " in ", folder))
          if (is.null(first_error)) first_error <<- conditionMessage(e)
          NULL
        }
      )
      if (!is.null(written)) {
        downloaded[[length(downloaded) + 1L]] <- written
      }
    }
  }

  if (length(failed) > 0L) {
    warning(
      "The attachments of ", length(failed), " message", if (length(failed) > 1L) "s",
      " could not be downloaded (", paste(utils::head(failed, 5L), collapse = "; "),
      if (length(failed) > 5L) "; ...", "): ", first_error, ".",
      call. = FALSE
    )
  }
  if (length(downloaded) == 0L) {
    return(empty_downloads())
  }
  out <- tibble::as_tibble(do.call(rbind, downloaded))
  assert_inside(out$path, dir)
  n_messages <- length(unique(paste(out$folder, out$msg_id)))
  message(
    "Downloaded ", nrow(out), " attachment", if (nrow(out) != 1L) "s", " from ",
    n_messages, " message", if (n_messages != 1L) "s",
    " into ", normalizePath(dir, mustWork = TRUE), "."
  )
  out
}

# Writes the decoded attachments of one message and describes what was written.
save_attachments <- function(result, msg_id, folder, dir, account, simplify) {
  if (!is.data.frame(result) || nrow(result) == 0L) {
    # The attachments were listed, so an empty answer means the message changed
    # since it was fetched.
    stop("the server returned none of the attachment parts that were listed", call. = FALSE)
  }
  target <- if (simplify) {
    dir
  } else {
    file.path(dir, safe_file_name(account$user), folder_dir_name(folder), paste0("UID", id_string(msg_id)))
  }
  dir.create(target, recursive = TRUE, showWarnings = FALSE)

  names <- disambiguate(vapply(result$filename, safe_file_name, "", USE.NAMES = FALSE), result$part)
  if (simplify) {
    names <- vapply(names, flat_file_name, "", folder = folder, msg_id = msg_id, USE.NAMES = FALSE)
  }
  paths <- file.path(target, names)
  for (j in seq_along(paths)) {
    assert_writable(paths[j])
    writeBin(result$content[[j]], paths[j])
  }
  paths <- normalizePath(paths, mustWork = TRUE)
  tibble::tibble(
    msg_id = msg_id, folder = folder, part = result$part,
    name = result$filename, content_type = result$type,
    size = as.numeric(file.size(paths)), path = paths
  )
}

# A folder name as one path component. mRpostman returns spaces as "%20". Every
# character a file name cannot hold is written as "%XX", with "%" itself escaped
# too, so different folders never share a name ("Archives/2024" and
# "Archives_2024" stay distinct). Leading and trailing dots are escaped as well.
folder_dir_name <- function(folder) {
  name <- gsub("%20", " ", folder, fixed = TRUE)
  escape <- function(chars) {
    vapply(chars, function(ch) paste0(sprintf("%%%02X", as.integer(charToRaw(ch))), collapse = ""), "")
  }
  escape_matches <- function(name, pattern) {
    m <- gregexpr(pattern, name)
    regmatches(name, m) <- lapply(regmatches(name, m), escape)
    name
  }
  name <- escape_matches(name, "[%\\\\/:*?\"<>|[:cntrl:]]")
  name <- escape_matches(name, "^\\.+|[. ]+$")
  safe_file_name(name)
}

# "<folder>_<msg_id>_<name>": the folder and id identify the message, so the name
# is the same on every run and different for every message. The length limit
# cuts the end of the name, never the prefix.
flat_file_name <- function(folder, msg_id, name) {
  safe_file_name(paste0(folder_dir_name(folder), "_", id_string(msg_id), "_", name))
}

# Attachments of one message with the same name each get their MIME part number
# (image.png becomes image-2.png and image-3_1.png), because they would
# overwrite each other. Case is ignored: macOS and Windows file systems ignore
# it.
disambiguate <- function(names, parts) {
  key <- tolower(names)
  clash <- key %in% key[duplicated(key)]
  for (j in which(clash)) {
    p <- split_extension(names[j])
    names[j] <- paste0(p$stem, "-", gsub(".", "_", parts[j], fixed = TRUE), p$ext)
  }
  names
}

# Existing files are overwritten, but never a directory and never through a link.
assert_writable <- function(path) {
  if (dir.exists(path) || is_link(path)) {
    stop("Will not overwrite \"", basename(path), "\": it is a directory or a link.", call. = FALSE)
  }
  invisible(path)
}

# Sys.readlink() is NA for a path that does not exist and nzchar(NA) is TRUE, so
# NA has to be checked first.
is_link <- function(path) {
  target <- Sys.readlink(path)
  !is.na(target) && nzchar(target)
}

# "report.pdf" -> report + .pdf; "backup.tar.gz" -> backup + .tar.gz; "README" -> README + "".
split_extension <- function(name) {
  m <- regmatches(
    name, regexec("^(.+?)(\\.tar\\.(?:gz|bz2|xz)|\\.[A-Za-z0-9]{1,10})$", name, perl = TRUE)
  )[[1L]]
  if (length(m) == 3L) list(stem = m[2L], ext = m[3L]) else list(stem = name, ext = "")
}

# A file name that is safe to create anywhere, whatever the sender wrote: no path
# separators or control characters, never hidden (a leading dot would let an
# attachment called .bashrc land in a home directory), no reserved Windows device
# names, no trailing dots or spaces, a bounded length with the extension kept.
safe_file_name <- function(x, fallback = "attachment") {
  if (length(x) != 1L || is.na(x)) {
    x <- ""
  }
  x <- gsub("[[:cntrl:]]", "", x)
  x <- gsub("[\\\\/:*?\"<>|]", "_", x)
  x <- sub("^\\.+", "_", trimws(x))
  x <- sub("[. ]+$", "", x)
  if (!nzchar(x)) {
    return(fallback)
  }
  if (grepl("^(CON|PRN|AUX|NUL|COM[0-9]|LPT[0-9])(\\.|$)", x, ignore.case = TRUE)) {
    x <- paste0("_", x)
  }
  if (nchar(x) > 180L) {
    p <- split_extension(x)
    x <- paste0(substr(p$stem, 1L, 180L - nchar(p$ext)), p$ext)
  }
  x
}

# The dots make `simplify` named-only; anything passed through them is an error.
reject_download_args <- function(...) {
  n <- ...length()
  if (n == 0L) {
    return(invisible())
  }
  nms <- ...names() %||% rep("", n)
  nms[is.na(nms)] <- ""
  if (any(!nzchar(nms))) {
    stop(
      "download_attachments() takes `mail`, `account` and `dir` by position; `simplify` ",
      "must be given by name.",
      call. = FALSE
    )
  }
  stop("Unused argument", if (n > 1L) "s", ": ", paste0("`", nms, "`", collapse = ", "), ".", call. = FALSE)
}

empty_downloads <- function() {
  tibble::tibble(
    msg_id = integer(0), folder = character(0), part = character(0), name = character(0),
    content_type = character(0), size = numeric(0), path = character(0)
  )
}

check_mail_table <- function(mail) {
  needed <- c("msg_id", "folder", "attachments")
  if (!is.data.frame(mail) || !all(needed %in% names(mail))) {
    missing <- if (is.data.frame(mail)) setdiff(needed, names(mail)) else needed
    stop(
      "`mail` must be a table from fetch_mail() with `msg_id`, `folder` and `attachments` ",
      "columns (missing: ", paste0("`", missing, "`", collapse = ", "), ").",
      call. = FALSE
    )
  }
  invisible(mail)
}

# Names are made safe before anything is written, so a path should never leave
# `dir`; this check is the guarantee. A path that resolves elsewhere is not
# reported as a success.
assert_inside <- function(paths, dir) {
  root <- normalizePath(dir, mustWork = TRUE)
  resolved <- normalizePath(paths, mustWork = FALSE)
  inside <- startsWith(resolved, paste0(root, .Platform$file.sep))
  if (!all(inside)) {
    stop(
      "Refusing to report files outside `dir` (", paste(basename(paths[!inside]), collapse = ", "), ").",
      call. = FALSE
    )
  }
  invisible(paths)
}
