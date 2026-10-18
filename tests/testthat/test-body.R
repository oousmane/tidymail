a_grave <- intToUtf8(224L)
e_acute <- intToUtf8(233L)
crlf <- function(...) paste(c(...), collapse = "\r\n")

test_that("a quoted-printable text/plain part is decoded, honouring its charset", {
  expect_equal(
    tidymail:::decode_body(
      "text/plain; charset=UTF-8", "quoted-printable",
      "Bonjour =C3=A0 tous, pr=C3=A9c=\r\n=C3=A9dent."
    ),
    paste0("Bonjour ", a_grave, " tous, pr", e_acute, "c", e_acute, "dent.")
  )
  expect_equal(
    tidymail:::decode_body("text/plain; charset=iso-8859-1", "quoted-printable", "caf=E9"),
    paste0("caf", e_acute)
  )
})

test_that("multipart/alternative prefers text/plain over text/html", {
  body <- crlf(
    "--b1", "Content-Type: text/plain; charset=utf-8", "Content-Transfer-Encoding: quoted-printable", "",
    "Plain =C3=A0 lire", "--b1", "Content-Type: text/html; charset=utf-8", "", "<p>HTML version</p>",
    "--b1--", ""
  )
  expect_equal(
    tidymail:::decode_body("multipart/alternative; boundary=\"b1\"", NA_character_, body),
    paste0("Plain ", a_grave, " lire")
  )
})

test_that("an HTML-only message is reduced to readable text", {
  # base64 of: <html><style>p{color:red}</style><body><p>Salut&nbsp;l&eacute;quipe</p><p>A &amp; B</p></body></html>
  b64 <- paste0(
    "PGh0bWw+PHN0eWxlPnB7Y29sb3I6cmVkfTwvc3R5bGU+PGJvZHk+PHA+U2FsdXQmbmJzcDtsJmVhY3V0",
    "ZTtxdWlwZTwvcD48cD5BICZhbXA7IEI8L3A+PC9ib2R5PjwvaHRtbD4="
  )
  body <- crlf(
    "--h", "Content-Type: text/html; charset=utf-8", "Content-Transfer-Encoding: base64", "",
    b64, "--h--", ""
  )
  expect_equal(
    tidymail:::decode_body("multipart/alternative; boundary=h", NA_character_, body),
    paste0("Salut l", e_acute, "quipe\n\nA & B")
  )
})

test_that("nested multipart/mixed picks the body and ignores attachments", {
  body <- crlf(
    "--outer", "Content-Type: multipart/alternative; boundary=\"inner\"", "",
    "--inner", "Content-Type: text/plain; charset=utf-8", "", "The real body",
    "--inner", "Content-Type: text/html", "", "<p>The real body</p>", "--inner--",
    "--outer", "Content-Type: text/plain; name=notes.txt", "Content-Disposition: attachment; filename=notes.txt", "",
    "attached notes, not the body",
    "--outer", "Content-Type: application/pdf", "Content-Transfer-Encoding: base64", "", "JVBERi0=",
    "--outer--", ""
  )
  expect_equal(tidymail:::decode_body("multipart/mixed; boundary=outer", NA_character_, body), "The real body")
})

test_that("a structure it can't read yields the raw body", {
  expect_equal(tidymail:::decode_body("multipart/mixed", NA_character_, "raw stuff"), "raw stuff")
  expect_equal(tidymail:::decode_body(NA_character_, NA_character_, "just text"), "just text")
  expect_true(is.na(tidymail:::decode_body("text/plain", NA_character_, NA_character_)))
})

test_that("invalid HTML code points preserve readable body text", {
  for (entity in c("&#55296;", "&#1114112;", "&#0;", "&#999999999999999;")) {
    expect_equal(
      tidymail:::decode_body("text/html", NA_character_, paste0("<p>Keep this text ", entity, "</p>")),
      paste0("Keep this text ", entity)
    )
  }
})

test_that("HTML entities are decoded once, never twice", {
  expect_equal(tidymail:::decode_html_entities("&amp;lt; and &lt;b&gt; and &amp;amp;"), "&lt; and <b> and &amp;")
})
