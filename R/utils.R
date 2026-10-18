# Helpers shared across the package. Helpers used by one feature live in that
# feature's file.

`%||%` <- function(x, y) if (is.null(x)) y else x

is_nonblank_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
}

validate_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", name, "` must be TRUE or FALSE.", call. = FALSE)
  }
}

month_abb_en <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

# Message ids as text. as.character(100000) is "1e+05", which matches neither
# mRpostman's result names nor a file name.
id_string <- function(x) format(x, scientific = FALSE, trim = TRUE)

# mRpostman writes an id into its FETCH commands and result names with
# as.character(), so UID 100000 would be requested as "1e+05". Fixed notation
# for the rest of the calling function avoids that for every id.
local_fixed_notation <- function(env = parent.frame()) {
  old <- options(scipen = 999)
  do.call(on.exit, list(bquote(options(.(old))), add = TRUE), envir = env)
  invisible()
}

format_bytes <- function(x) {
  if (x >= 1024) paste0(format(round(x / 1024, 1), trim = TRUE), " KB") else paste0(x, " bytes")
}
