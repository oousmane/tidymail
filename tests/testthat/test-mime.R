# Accented characters are built with intToUtf8() so the comparisons don't
# depend on the source file's encoding or the test runner's locale.
e_acute <- intToUtf8(233L)
cafe <- paste0("caf", e_acute)
precedent <- paste0("pr", e_acute, "c", e_acute, "dent")

test_that("decode_mime_header decodes encoded words and preserves unencoded headers", {
  expect_equal(tidymail:::decode_mime_header("=?UTF-8?Q?pr=C3=A9c=C3=A9dent?="), precedent)
  expect_equal(tidymail:::decode_mime_header("=?UTF-8?Q?Bonjour_le_monde?="), "Bonjour le monde")
  expect_equal(tidymail:::decode_mime_header("=?UTF-8?B?Y2Fmw6k=?="), cafe)
  expect_equal(tidymail:::decode_mime_header("pr=C3=A9c=C3=A9dent"), "pr=C3=A9c=C3=A9dent")
  expect_equal(tidymail:::decode_mime_header("Hello world"), "Hello world")
  expect_true(is.na(tidymail:::decode_mime_header(NA_character_)))
})

# The shapes below are what mRpostman::decode_mime_header() gets wrong
# (verified against 3.0.0): an encoded word embedded in other text, which is
# the normal "Name <address>" header.
test_that("an encoded word inside a display name + address is decoded", {
  expect_equal(
    tidymail:::decode_mime_header("DABIRE =?UTF-8?Q?Domi=C3=A9zag?= <dabire.jacques@meteoburkina.bf>"),
    paste0("DABIRE Domi", e_acute, "zag <dabire.jacques@meteoburkina.bf>")
  )
})

test_that("adjacent encoded words fold their whitespace and decode by their own encoding", {
  expect_equal(tidymail:::decode_mime_header("=?UTF-8?Q?Bonjour_?= =?UTF-8?Q?le_monde?="), "Bonjour le monde")
  expect_equal(tidymail:::decode_mime_header("=?UTF-8?Q?a?= =?UTF-8?B?Yg==?="), "ab")
})

test_that("extract_header_field unfolds continuation lines and is NA when absent", {
  raw <- paste0(
    "From: =?UTF-8?Q?pr=C3=A9c=C3=A9dent?= <dabire.jacques@meteoburkina.bf>\r\n",
    "Subject: =?UTF-8?Q?pr=C3=A9c=C3=A9dent?=\r\n",
    " =?UTF-8?Q?_le_retour?=\r\n\r\n"
  )
  expect_equal(
    tidymail:::extract_header_field(raw, "From"),
    "=?UTF-8?Q?pr=C3=A9c=C3=A9dent?= <dabire.jacques@meteoburkina.bf>"
  )
  subject <- tidymail:::extract_header_field(raw, "Subject")
  expect_equal(tidymail:::decode_mime_header(subject), paste0(precedent, " le retour"))
  expect_true(is.na(tidymail:::extract_header_field(raw, "Cc")))
})

test_that("qp_decode leaves already-multibyte text intact", {
  expect_identical(tidymail:::qp_decode(cafe, FALSE), charToRaw(cafe))
})

test_that("base64_decode is correct for every padding length", {
  # vectors from the BSD `base64` tool, on the bytes 31*(1..n) mod 256
  encoded <- c(`1` = "Hw==", `2` = "Hz4=", `3` = "Hz5d")
  for (n in as.integer(names(encoded))) {
    expect_identical(tidymail:::base64_decode(encoded[[as.character(n)]]), as.raw(seq_len(n) * 31L))
  }
})

test_that("literal equals signs in unencoded headers are preserved", {
  for (header in c("Budget=2026", "https://example.com/?token=AB12", "x=20", "a=40b@example.com")) {
    expect_identical(tidymail:::decode_mime_header(header), header)
  }
})

test_that("a download cut in the middle of a character does not turn the text into Latin-1 mojibake", {
  cafe_bytes <- charToRaw(paste0("caf", e_acute))                     # 63 61 66 c3 a9
  expect_equal(tidymail:::bytes_to_string(cafe_bytes, "UTF-8"), paste0("caf", e_acute))
  # cut after the first byte of "é": drop the half character, keep the rest as UTF-8
  expect_equal(tidymail:::bytes_to_string(cafe_bytes[1:4], "UTF-8"), "caf")
  # cut inside a 3-byte character
  expect_equal(tidymail:::bytes_to_string(c(charToRaw("a"), as.raw(0xe2), as.raw(0x82)), "UTF-8"), "a")
  # genuinely invalid bytes in the middle still fall back to Latin-1
  expect_equal(tidymail:::bytes_to_string(as.raw(c(0x63, 0xe9, 0x66, 0x65)), "UTF-8"), paste0("c", e_acute, "fe"))
})

utc <- function(s) as.POSIXct(s, tz = "UTC")

test_that("RFC 5322 dates parse to the right UTC instant (offsets, optional parts, named zones)", {
  expect_equal(tidymail:::parse_mail_date("Mon, 7 May 2001 08:41:00 -0700"), utc("2001-05-07 15:41:00"))
  expect_equal(tidymail:::parse_mail_date("Thu, 1 Sep 2026 11:42:46 +0530"), utc("2026-09-01 06:12:46"))
  expect_equal(tidymail:::parse_mail_date("7 May 2001 08:41 GMT"), utc("2001-05-07 08:41:00"))
  expect_equal(tidymail:::parse_mail_date("Mon, 7 May 2001 08:41:00 +0000 (UTC)"), utc("2001-05-07 08:41:00"))
})

test_that("parsing does not depend on the session locale", {
  old <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", old))
  # skipped silently where no French locale is installed
  if (nzchar(suppressWarnings(Sys.setlocale("LC_TIME", "fr_FR.UTF-8")))) {
    expect_equal(tidymail:::parse_mail_date("Mon, 7 Sep 2026 08:41:00 +0000"), utc("2026-09-07 08:41:00"))
  } else {
    skip("no fr_FR.UTF-8 locale on this machine")
  }
})

test_that("a bad or missing date is NA for that element only, never an error", {
  out <- tidymail:::parse_mail_date(c("Mon, 7 May 2001 08:41:00 -0700", "not a date", NA, ""))
  expect_equal(out[1], utc("2001-05-07 15:41:00"))
  expect_true(all(is.na(out[2:4])))
})
