#' Check whether strings look like email addresses
#'
#' Checks the syntax of plain addresses such as `john.doe@example.com`, and
#' optionally whether the domain can receive mail. By default it never touches
#' the network, so it is safe to call anywhere, including offline.
#'
#' @param x A character vector of addresses.
#' @param mxlookup Whether to also look up the domain's MX record, which needs
#'   the 'pingr' package and a working DNS. Default `FALSE`. A domain whose only
#'   MX record is `"."` (it states that it accepts no mail), and a failed lookup
#'   of any kind, count as no MX. Each domain is looked up once.
#' @param server Optional DNS server to ask, such as `"8.8.8.8"`. Passed to
#'   `pingr::nsl()`.
#'
#' @return A logical vector the same length as `x`: `TRUE` for a valid address,
#'   `FALSE` for an invalid one, and `NA` where `x` is `NA`.
#'
#' @details
#' The syntax check is the RFC 5322 `addr-spec` form, written in base R. It
#' accepts only a bare address: a display name (`Bob <bob@example.com>`), a
#' domain with non-ASCII characters, and an address literal
#' (`user@[127.0.0.1]`) are all rejected.
#'
#' Neither check can confirm that a mailbox exists. A domain with no MX record
#' can still receive mail through its A record, so `mxlookup = TRUE` is a hint,
#' not proof.
#'
#' @examples
#' is_address(c("john.doe@example.com", "not an address", NA))
#' \dontrun{
#' is_address("john.doe@example.com", mxlookup = TRUE)
#' }
#'
#' @export
is_address <- function(x, mxlookup = FALSE, server = NULL) {
  if (is.logical(x) && all(is.na(x))) {
    x <- as.character(x)
  }
  if (!is.character(x)) {
    stop("`x` must be a character vector of email addresses.", call. = FALSE)
  }
  validate_flag(mxlookup, "mxlookup")
  if (!is.null(server) && !is_nonblank_string(server)) {
    stop("`server` must be NULL or a single DNS server name.", call. = FALSE)
  }

  out <- rep(NA, length(x))
  known <- !is.na(x)
  out[known] <- vapply(x[known], address_syntax_ok, logical(1), USE.NAMES = FALSE)

  if (mxlookup && any(out %in% TRUE)) {
    if (!requireNamespace("pingr", quietly = TRUE)) {
      stop("`mxlookup = TRUE` needs the 'pingr' package: install.packages(\"pingr\").", call. = FALSE)
    }
    domains <- sub("^.*@", "", x[out %in% TRUE])
    found <- vapply(unique(domains), has_mx, logical(1), server = server)
    out[out %in% TRUE] <- unname(found[domains])
  }
  out
}

address_syntax_ok <- function(addr) {
  if (nchar(addr) > 254) {
    return(FALSE)
  }
  at <- gregexpr("@", addr, fixed = TRUE)[[1]]
  if (length(at) != 1L) {
    return(FALSE)
  }
  local <- substr(addr, 1L, at - 1L)
  domain <- substr(addr, at + 1L, nchar(addr))

  nchar(local) %in% 1:64 &&
    grepl("^[A-Za-z0-9!#$%&'*+/=?^_`{|}~.-]+$", local) &&
    !grepl("^\\.|\\.$|\\.\\.", local) &&
    nchar(domain) <= 253 &&
    grepl("^([A-Za-z0-9]([A-Za-z0-9-]*[A-Za-z0-9])?\\.)+[A-Za-z]{2,}$", domain)
}

# TRUE if `domain` publishes a usable MX record. A lookup that fails (no DNS, no
# such domain, timeout) is FALSE, and so is the RFC 7505 "null MX" ("."), which
# is how a domain says it accepts no mail.
has_mx <- function(domain, server = NULL) {
  mx <- tryCatch(
    pingr::nsl(domain, server = server, type = 15L),
    error = function(e) NULL
  )
  answer <- mx$answer
  if (is.null(answer) || nrow(answer) == 0L) {
    return(FALSE)
  }
  any(!trimws(as.character(answer$data)) %in% c("", "."))
}
