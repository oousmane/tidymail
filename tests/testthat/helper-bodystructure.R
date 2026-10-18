# BODYSTRUCTURE fixtures, run through mRpostman's own parser so they have exactly
# the shape ImapCon$fetch_bodystructure() returns: a first column `uid`, then
# one row per MIME part (containers have part = NA).
bs_text <- '("text" "plain" ("charset" "utf-8") NIL NIL "7bit" 20 1 NIL NIL NIL NIL)'
bs_logo <- paste0(
  '("image" "png" ("name" "logo.png") "<cid1>" NIL "base64" 900 NIL ',
  '("inline" ("filename" "logo.png")) NIL NIL)'
)
bs_pdf <- paste0(
  '("application" "pdf" ("name" "report.pdf") NIL NIL "base64" 4096 NIL ',
  '("attachment" ("filename" "report.pdf")) NIL NIL)'
)
bs_zip_no_disposition <- '("application" "zip" ("name" "data.zip") NIL NIL "base64" 2048 NIL NIL NIL NIL)'
bs_pdf_no_name <- '("application" "pdf" NIL NIL NIL "base64" 4096 NIL ("attachment" NIL) NIL NIL)'
bs_pdf_encoded_name <- paste0(
  '("application" "pdf" NIL NIL NIL "base64" 100 NIL ',
  '("attachment" ("filename" "=?UTF-8?Q?r=C3=A9sum=C3=A9.pdf?=")) NIL NIL)'
)

bs_mixed <- function(...) {
  paste0("BODYSTRUCTURE (", paste(c(...), collapse = ""), ' "mixed" ("boundary" "b") NIL NIL NIL)')
}
bs_single <- function(part) paste0("BODYSTRUCTURE ", part)

bodystructure_df <- function(uid, structure) {
  parts <- suppressWarnings(mRpostman::parse_bodystructure(structure))
  out <- cbind(data.frame(id = rep(as.integer(uid), nrow(parts))), parts)
  names(out)[1] <- "uid"
  out
}
