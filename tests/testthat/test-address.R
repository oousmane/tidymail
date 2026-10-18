test_that("is_address checks syntax only by default, is vectorised, and keeps NA", {
  skip_if_not_installed("pingr")
  # a lookup here would mean every constructor needs a network again
  testthat::local_mocked_bindings(nsl = function(...) stop("no network wanted"), .package = "pingr")
  expect_equal(
    is_address(c("user@example.com", "first.last+tag@sub.example.co.uk", "not an address", "user@no-dot", NA)),
    c(TRUE, TRUE, FALSE, FALSE, NA)
  )
  # a bare address only: no display name, no non-ASCII domain, no line break
  expect_false(any(is_address(c("Bob <bob@example.com>", "a@b\u00fccher.de", "a@b.com\n", "a@[127.0.0.1]"))))
  expect_equal(is_address(NA), NA)
  expect_equal(is_address(character(0)), logical(0))
})

test_that("is_address rejects input that is not a character vector", {
  expect_error(is_address(123), "character vector")
  expect_error(is_address(list("a@b.com")), "character vector")
  expect_error(is_address("a@b.com", mxlookup = NA), "mxlookup")
  expect_error(is_address("a@b.com", server = 1), "server")
})

test_that("mxlookup asks DNS once per domain, only for well-formed addresses, and fails closed", {
  skip_if_not_installed("pingr")
  asked <- character(0)
  answers <- list(
    "good.com" = data.frame(data = "mx.good.com"),
    "nullmx.com" = data.frame(data = "."),    # RFC 7505: this domain accepts no mail
    "empty.com" = data.frame()
  )
  testthat::local_mocked_bindings(nsl = function(domain, ...) {
    asked <<- c(asked, domain)
    if (domain == "down.com") stop("DNS failure")
    list(answer = answers[[domain]])
  }, .package = "pingr")

  x <- c("a@good.com", "b@good.com", "c@nullmx.com", "d@empty.com", "e@down.com", "bad address", NA)
  expect_equal(is_address(x, mxlookup = TRUE), c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, NA))
  expect_setequal(asked, c("good.com", "nullmx.com", "empty.com", "down.com"))
  expect_equal(sum(asked == "good.com"), 1L)
})
