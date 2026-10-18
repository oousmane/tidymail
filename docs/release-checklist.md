# tidymail first release manual tests

Run these cases against the current package before the first release. Work
through one case at a time and record PASS, FAIL, or SKIP in
[release-results.csv](release-results.csv). A successful R call does not prove
that a message arrived: sending cases also require inspection in a mail client.
Record the provider, operating system, and package versions with your results.
Cases marked optional require a second account, another platform, or a controlled
server; record the reason when skipping them.

Use test accounts and recipients you control. The sending cases deliver real
messages. Credential cases save, replace, or delete the named test account's
keychain entries. The examples contain password placeholders; keep real values
out of shared scripts, history, saved sessions, and test reports. No steps here
run automatically.

## Preparation

From an installed package, use `library(tidymail)`. When testing this checkout,
run `devtools::load_all(".")` from the project root instead; repeat that after
restarting R so you do not accidentally test an older installed version.

Install `keyring` and `dplyr` before the live cases if needed.

Run this setup first. Replace the configuration and change `test_tag` for each
complete run. After restarting R, reuse the same tag and rerun the setup, which
does not save passwords or send mail.

```r
library(tidymail)
library(dplyr)

cfg <- list(
  email = "me@example.com",
  smtp_host = "smtp.example.com",
  smtp_port = 587,
  smtp_user = "me@example.com",
  imap_host = "imap.example.com",
  imap_port = 993,
  imap_user = "me@example.com",
  other_folder = "Archives"
)

test_tag <- "tidymail-check-20261016-a"  # Choose a unique tag for this run.
subject_for <- function(case) paste(test_tag, case)
out_dir <- file.path(tempdir(), "tidymail-release-test")
dir.create(out_dir, showWarnings = FALSE)

report <- file.path(out_dir, "report.txt")
writeLines(c("tidymail attachment test", "Expected content: 12345"), report)
results_file <- file.path(out_dir, "results.csv")
write.csv(data.frame(station = c("A", "B"), rain = c(12, 18)),
          results_file, row.names = FALSE)
unicode_file <- file.path(out_dir, "rapport été.txt")
writeLines("Résumé : données vérifiées.", unicode_file, useBytes = TRUE)
empty_file <- file.path(out_dir, "empty.txt")
file.create(empty_file)

make_smtp <- function(...) smtp_account(
  "custom", email = cfg$email, user = cfg$smtp_user,
  host = cfg$smtp_host, port = cfg$smtp_port,
  pass_envvar = "TIDYMAIL_RELEASE_SMTP_PASSWORD", ...
)
make_imap <- function(...) imap_account(
  "custom", email = cfg$email, user = cfg$imap_user,
  host = cfg$imap_host, port = cfg$imap_port,
  pass_envvar = "TIDYMAIL_RELEASE_IMAP_PASSWORD", ...
)

# Reports an expected error; fails if the expression unexpectedly succeeds.
check_error <- function(expr) {
  error <- tryCatch({ force(expr); NULL }, error = identity)
  if (is.null(error)) stop("Expected an error, but the call succeeded.")
  message(conditionMessage(error))
  invisible(error)
}

# Reports an expected warning; fails if the expression runs without one.
check_warning <- function(expr) {
  caught <- NULL
  value <- withCallingHandlers(expr, warning = function(w) {
    caught <<- w
    invokeRestart("muffleWarning")
  })
  if (is.null(caught)) stop("Expected a warning, but the call was silent.")
  message(conditionMessage(caught))
  invisible(value)
}

# Define these now; use them after registering credentials below.
smtp <- make_smtp()
imap <- make_imap()
send_case <- function(case, body = "Hello from tidymail.", ...) {
  build_msg(cfg$email, subject_for(case), body, ...) |> send_mail(smtp)
}
fetch_case <- function(case, ...) {
  fetch_mail(imap, folder = "INBOX", subject = subject_for(case), ...)
}
```

Keep the dedicated environment variables unset while testing keychain lookup.
If they already contain values, preserve them separately before clearing them.
Do not unset unrelated production variables.

## Installation and credentials

### I01 Load a clean installation

Install the package into a fresh test library, then load it in a fresh R
session. If using the checkout, `devtools::install(".", lib = test_lib)` can
install into a directory you choose. Check:

```r
packageVersion("tidymail")
packageVersion("blastula")
packageVersion("mRpostman")
packageVersion("keyring")
sessionInfo()
stopifnot(all(c("send_mail", "fetch_mail", "build_msg", "preview_mail") %in%
              getNamespaceExports("tidymail")))
```

Expected: loading succeeds without missing dependencies; the installed version
exports the refined functions. Record whether this is a source-tree or installed
package run.

### I02 Run the automated package checks

From the project root in a development environment:

```r
devtools::test()
devtools::check(document = FALSE, manual = FALSE)
```

Expected: all assertions pass, and package checks finish without errors,
warnings, or notes. Record any platform/setup issue separately; do not count
an interrupted check as a pass. These automated tests do not send live mail or
write to your native keychain.

### I03 Read installed help and examples

```r
help("send_mail", package = "tidymail")
help("fetch_mail", package = "tidymail")
help("preview_mail", package = "tidymail")
help("smtp_account", package = "tidymail")
```

Expected: help opens, argument names match the installed API, and the README
quick start can be followed without undocumented setup. Repeat a simple send,
preview, and fetch against the installed package if earlier cases used
`devtools::load_all()`.

### C01 Save separate SMTP and IMAP passwords

```r
smtp <- make_smtp()   # building an account must not touch the keychain
imap <- make_imap()
register_creds(smtp, password = "your-smtp-app-password")
register_creds(imap, password = "your-imap-app-password")
print(smtp)
print(imap)
str(smtp)
str(imap)
stopifnot(!"password" %in% names(smtp), !"password" %in% names(imap))
```

Expected: building the two accounts produces no keychain permission request;
`register_creds()` saves a native keychain entry for each transport. No tidymail
password prompt appears. Returned objects and printed output contain no password.
The operating system may request keychain permission when registering. Building
the accounts again later must not overwrite or need the saved entries.

### C02 Retrieve saved passwords after restarting R

Restart R, reload the current package, rerun Preparation with the same settings,
and rebuild the accounts (do not register again):

```r
smtp <- make_smtp()
imap <- make_imap()
send_case("C02")
fetch_case("C02")  # Retry manually after delivery if initially empty.
```

Expected: sending and fetching succeed without supplying passwords again. Confirm
the message arrives. No startup environment variable should provide the password
in this case.

### C03 One shared account for identical logins

Only run this if both transports actually use the same username and password.

```r
both <- mail_account(
  "custom", email = cfg$email,
  smtp_host = cfg$smtp_host, smtp_port = cfg$smtp_port,
  imap_host = cfg$imap_host, imap_port = cfg$imap_port,
  pass_envvar = "TIDYMAIL_RELEASE_SHARED_PASSWORD"
)
register_creds(both, password = "your-shared-app-password")
build_msg(cfg$email, subject_for("C03"), "Shared login") |> send_mail(both)
fetch_mail(both, subject = subject_for("C03"))
```

Expected: the same account object sends and reads. After restarting R, rebuilding
it (without registering again) also works. These entries may replace those from C01 when
the user and host match; only run this for genuinely shared logins.

### C04 Configure only one transport

```r
smtp_only <- mail_account("custom", cfg$email,
  smtp_host = cfg$smtp_host, smtp_port = cfg$smtp_port)
imap_only <- mail_account("custom", cfg$email,
  imap_host = cfg$imap_host, imap_port = cfg$imap_port)
check_error(fetch_mail(smtp_only))
check_error(send_mail(build_msg(cfg$email, "Test", "Test"), imap_only))
```

Expected: constructing either object works. Using the unavailable transport
fails before connecting, naming the missing SMTP or IMAP host. If the account
logs in using its email, also verify sending with `smtp_only` and fetching with
`imap_only`. Otherwise use the dedicated constructors with `user` overrides.

### C05 Reject an account of the wrong transport

```r
check_error(fetch_mail(smtp))
check_error(send_mail(build_msg(cfg$email, "Test", "Test"), imap))
```

Expected: errors identify the required account type; no message is sent.

### C06 A missing password produces an actionable error

Use a test identity that has never been registered:

```r
missing <- smtp_account("custom", "unregistered@example.com",
  host = cfg$smtp_host, user = paste0("unregistered-", test_tag),
  pass_envvar = "TIDYMAIL_RELEASE_UNSET_PASSWORD")
stopifnot(Sys.getenv("TIDYMAIL_RELEASE_UNSET_PASSWORD") == "")
check_error(send_mail(build_msg(cfg$email, "Test", "Test"), missing))
```

Expected: an error explains password setup, environment variables, and keychain
registration. It does not attempt authentication with a blank password.

### C07 Environment passwords without persistent storage

Run this after the other keychain-dependent cases, or on a separate test account.
`forget_creds()` removes these test entries; the final two calls restore them.

```r
forget_creds(smtp)
forget_creds(imap)
Sys.setenv(TIDYMAIL_RELEASE_SMTP_PASSWORD = "your-smtp-app-password")
Sys.setenv(TIDYMAIL_RELEASE_IMAP_PASSWORD = "your-imap-app-password")
send_case("C07")
fetch_case("C07")  # Retry manually after delivery if necessary.
Sys.unsetenv(c("TIDYMAIL_RELEASE_SMTP_PASSWORD", "TIDYMAIL_RELEASE_IMAP_PASSWORD"))
check_error(send_case("C07-missing"))
register_creds(smtp, password = "your-smtp-app-password")
register_creds(imap, password = "your-imap-app-password")
```

Expected: environment-only authentication works, and stops working when the
variables are removed. Sending through an environment variable does not save it
in the keychain. Ensure the cleanup and restoration run even if an earlier step
fails.

### C08 Environment values take precedence over keychain values

Optional: one failed authentication attempt on a test account. Skip if your
server's account-lockout policy makes this inappropriate. A correct password
must already be saved from C01.

```r
local({
  old <- Sys.getenv("TIDYMAIL_RELEASE_SMTP_PASSWORD", unset = NA_character_)
  on.exit(if (is.na(old)) Sys.unsetenv("TIDYMAIL_RELEASE_SMTP_PASSWORD") else
    Sys.setenv(TIDYMAIL_RELEASE_SMTP_PASSWORD = old))
  Sys.setenv(TIDYMAIL_RELEASE_SMTP_PASSWORD = "intentionally-wrong-test-password")
  check_error(send_case("C08-wrong-password"))
})
send_case("C08-restored")
```

Expected: the first call fails authentication despite a valid saved password.
After restoring the environment, sending succeeds. No incorrect password is
persisted or printed.

### C09 Reject unsupported keyring fallback backends

```r
local({
  old <- options(keyring_backend = "env")
  on.exit(options(old))
  check_error(register_creds(make_smtp(), password = "dummy-not-a-real-secret"))
})
```

Expected: an error requires a native system keychain. The dummy password is not
saved through the environment backend.

### C10 Replace a saved password explicitly

```r
register_creds(smtp, password = "your-current-smtp-app-password")
send_case("C10")
```

Expected: registration and subsequent sending succeed, without printing the
password. For an actual rotation test, rotate the test account's app password
through its provider, register the replacement, and verify that it works.

### C11 Delete a saved password

Run near the end, after other live tests. Keep the dedicated environment
variables unset.

```r
forget_creds(smtp)
check_error(send_case("C11-deleted"))
forget_creds(smtp)  # Deleting an absent entry is harmless.
register_creds(smtp, password = "your-smtp-app-password")
send_case("C11-restored")
```

Expected: deletion disables keychain authentication; repeating deletion reports
nothing to remove. Re-registering restores access. The IMAP account still works.

### C12 Unavailable or locked native keychain

Optional: use a disposable OS account or test machine, not your normal keychain.
Test with its credential service unavailable or access denied.

Expected: saving reports a clear error and never falls back to another store.
An OS unlock/permission dialog is acceptable. Cancellation must not report a
successful save. Deletion failures must not be reported as successful removal.
Environment authentication should still work without accessing the keychain.

### C13 Different login names and providers

Optional: configure actual SMTP and IMAP services with different `user` values,
passwords, or providers. Use `smtp_account()` and `imap_account()` separately.

Expected: sending uses only the SMTP login; reading uses only the IMAP login.
An SMTP password never substitutes for an IMAP password. Note that saved passwords
are keyed by transport and `user@host`; C03 is unsuitable for different logins.

## Drafts and previews

`build_msg()` takes `to, subject, body` first, then named extras, and
`send_mail(msg, account)` delivers the result. A draft has no sender: it is sent
from the account's own address, which a preview does not show; the real address
appears on delivery.

### D01 Build and print a draft without connecting

```r
msg <- build_msg(cfg$email, subject_for("D01"), "## Hello\n\n**Bold** text.")
msg
stopifnot(inherits(msg, "tidymail_msg"))
```

Expected: a readable summary prints. No email is sent and no account or password is
requested. Draft construction must also work without network access.

### D02 Add files without changing the original draft

```r
attached <- msg |> add_attachment(report) |> add_attachment(results_file)
stopifnot(length(msg$attachments) == 0L, length(attached$attachments) == 2L)
attached
```

Expected: the new draft lists two files; the original draft stays unchanged.

### D03 Preview Markdown interactively

```r
attached |> preview_mail(browse = TRUE)
```

Expected: the RStudio Viewer or browser shows the subject, recipients,
attachment names, a heading, and bold text. It does not send mail. Files are
listed by name; previewing does not embed or open attachment contents.

### D04 Preview HTML and address metadata

```r
html_draft <- build_msg(cfg$email, subject_for("D04"),
  "<h2>Résumé</h2><p>Text with <strong>emphasis</strong>.</p>",
  cc = cfg$email, bcc = cfg$email, use_md = FALSE)
preview_mail(html_draft, browse = TRUE)
```

Expected: HTML renders rather than appearing as literal markup. Cc and Bcc are
visible in your local preview. This does not imply Bcc should appear in a
recipient's delivered headers.

### D05 Save a preview without opening it

```r
preview_path <- file.path(out_dir, "preview.html")
returned <- withVisible(preview_mail(attached, file = preview_path, browse = FALSE))
stopifnot(file.exists(preview_path), !returned$visible,
          identical(returned$value, attached))
```

Expected: the file is created, no viewer opens, and the original draft is
returned invisibly. Open the file manually to inspect the layout. The saved
page includes message text and addresses, so do not publish it unintentionally.

### D06 Preview accented characters and literal HTML in headers

```r
special <- build_msg(cfg$email, "Résumé <test> & café",
                    "Bonjour à tous — données vérifiées.")
preview_mail(special, browse = TRUE)
```

Expected: accents and punctuation display correctly; `<test>` appears as text
in the subject, not as a page element. Inspect in both RStudio and a standalone
browser if available. Preview appearance can differ from the recipient's client.

### D07 Keep attachment paths usable after changing directories

```r
local({
  old <- getwd()
  on.exit(setwd(old))
  setwd(out_dir)
  relative <- build_msg(cfg$email, subject_for("D07"), "Relative file") |>
    add_attachment("report.txt")
  setwd(old)
  preview_mail(relative, browse = FALSE)
  stopifnot(file.exists(relative$attachments))
})
```

Expected: the file is still found because its path was resolved when attached.

## Sending live mail

Send only to recipients you control. Open the received messages in a mail client
and compare the visible content and source headers. Use a second controlled
mailbox for S04; duplicate addresses cannot establish that Bcc stays hidden.

### S01 Send directly, from the account's address

```r
sent <- withVisible(send_case("S01"))
stopifnot(!sent$visible)
```

Expected: one message arrives with the correct subject and body. Its From address
matches `cfg$email`. A success result means the SMTP server accepted the message;
verify actual delivery separately.

### S02 Render Markdown

```r
send_case("S02", "## Monthly report\n\n**Bold**, *italic*, and a list:\n\n- One\n- Two")
```

Expected: the heading, emphasis, and list render correctly in the recipient's
mail client. Compare with `preview_mail()` for the same draft.

### S03 Render HTML directly

```r
send_case("S03", "<h2>HTML report</h2><p>A <strong>bold</strong> result.</p>", use_md = FALSE)
```

Expected: HTML is rendered, not displayed as escaped tags.

### S04 Deliver to multiple recipients with Cc and Bcc

Optional: set three distinct addresses that you control.

```r
to_two <- "second-controlled@example.com"
cc_address <- "cc-controlled@example.com"
bcc_address <- "bcc-controlled@example.com"
build_msg(c(cfg$email, to_two), subject_for("S04"), "Recipient test",
          cc = cc_address, bcc = bcc_address) |>
  send_mail(smtp)
```

Expected: all four mailboxes receive the message. To/Cc recipients do not see
the Bcc address in the delivered headers. Do not expect IMAP to recover Bcc from
the received message when the sender intentionally omitted it.

### S05 Attach one text file

```r
send_case("S05", "One attached text file.", attachments = report)
```

Expected: exactly one attachment arrives. Open it and verify the two setup lines,
including `Expected content: 12345`.

### S06 Attach several files

```r
send_case("S06", "Two files.", attachments = c(report, results_file))
```

Expected: exactly two attachments arrive, with correct filenames and contents.
Open the CSV and verify station/rain values.

### S07 Accents and an accented filename

```r
build_msg(cfg$email, paste(test_tag, "S07 Résumé été"),
  "Bonjour à tous. Données vérifiées : 12 °C, 20 €.",
  attachments = unicode_file) |>
  send_mail(smtp)
```

Expected: subject, body, filename, and file content preserve accented characters.

### S08 Send an empty attachment

```r
send_case("S08", "Empty-file test.", attachments = empty_file)
```

Expected: a zero-byte file arrives as an attachment, or the provider reports a
specific policy rejection. Silent omission is a failure.

### S09 Send a draft in a pipe

```r
build_msg(cfg$email, subject_for("S09"), "Draft workflow") |>
  add_attachment(report) |>
  send_mail(smtp)
```

Expected: exactly one message arrives with the expected attachment.

### S10 Send a named draft

```r
named <- build_msg(cfg$email, subject_for("S10"), "Named draft workflow")
send_mail(account = smtp, msg = named)
```

Expected: one message arrives; no message fields are lost.

### S11 Preview in the middle of the pipe, then send

```r
build_msg(cfg$email, subject_for("S11"), "Preview then send") |>
  preview_mail(browse = FALSE) |>
  send_mail(smtp)
```

Expected: a "Preview saved to" message appears, and one message arrives with the
correct sender (the account's address) and body. `preview_mail()` returns the draft, so sending continues.

### S12 The sender is the account's address; `from` is refused

```r
check_error(build_msg(cfg$email, subject_for("S12"), "Alias test", from = "alias@example.com"))
check_error(build_msg(cfg$email, subject_for("S12"), "Alias test", "alias@example.com"))
check_error(send_mail(build_msg(cfg$email, "Test", "Test"), smtp, from = "alias@example.com"))
build_msg(cfg$email, subject_for("S12"), "Sender test") |> send_mail(smtp)
```

Expected: the first and third errors say `from` is gone and the sender is the
account's email address; the second says the extras after `body` must be named,
so an old positional `from` cannot become a copy recipient. The message sent last
arrives from `cfg$email`. To test an alias, make an account whose `email` is the
alias (with its own `user` if the login differs) and send through it.

### S13 Explicit empty strings

```r
build_msg(cfg$email, "", "") |> send_mail(smtp)
```

Expected: the package accepts explicit empty strings and the server either
accepts them or provides a clear policy error. Missing values (`NA`) are tested
separately and must be rejected.

### S14 Compare local and received content

```r
comparison <- build_msg(cfg$email, subject_for("S14"),
  "## Comparison\n\n**Bold** and café.") |> add_attachment(report)
preview_mail(comparison, browse = TRUE)
send_mail(comparison, smtp)
```

Expected: the same subject, recipients, body content, and attachment names appear
in preview and delivery. Cosmetic differences between clients are acceptable.

## Retrieving live mail

Wait for the sent fixtures to arrive before these cases. Retrying fetches is
safe; do not resend fixtures just because delivery is delayed. Duplicate sends
produce duplicate matching messages, so most checks require at least one match.

### F01 Retrieve headers and bodies

```r
mail <- fetch_mail(imap, folder = "INBOX", subject = test_tag)
stopifnot(nrow(mail) > 0L, inherits(mail, "tbl_df"))
mail |> select(msg_id, folder, date, from, to, subject, body)
```

Expected: one row per matching message, decoded headers, readable bodies, and no
unexpected changes to message read/unread flags. Compare several rows to the mail
client. Unexpected flag changes are a failure.

### F02 Preserve a stable schema and UTC dates

```r
stopifnot(identical(names(mail), c("msg_id", "folder", "date", "from", "to", "cc",
                                 "bcc", "subject", "body", "has_attachment", "attachments")))
stopifnot(is.logical(mail$has_attachment))
stopifnot(inherits(mail$date, "POSIXct"), identical(attr(mail$date, "tzone"), "UTC"))
```

Expected: every fetch has these columns. Missing optional address fields may be
`NA`. Compare a known Date header and its UTC offset against the returned instant.

### F03 Fetch without bodies

```r
headers <- fetch_mail(imap, folder = "INBOX", subject = test_tag, include_body = FALSE)
stopifnot(nrow(headers) > 0L, all(is.na(headers$body)))
```

Expected: subject, addresses, date, and attachment metadata remain available;
`body` is `NA` for all rows. Compare attachment counts against F01 for the same
UIDs.

### F04 Filter by sender on the server

```r
from_me <- fetch_mail(imap, folder = "INBOX", from = cfg$email,
                      subject = subject_for("S01"))
stopifnot(nrow(from_me) > 0L)
```

Expected: the matching self-sent message is returned. If a gateway rewrites the
From header, use the actual stored sender and record the rewrite.

### F05 Match literal equals signs in subjects

First send this fixture:

```r
build_msg(cfg$email, paste(test_tag, "F05 Budget=2026"), "Literal header test") |>
  send_mail(smtp)
```

Once it arrives:

```r
equals <- fetch_mail(imap, subject = paste(test_tag, "F05"))
stopifnot(nrow(equals) > 0L, all(grepl("Budget=2026", equals$subject, fixed = TRUE)))
```

Expected: literal equals signs are preserved, rather than decoded as
quoted-printable escapes.

### F06 Include both date endpoints

Set the fixture day to the calendar date in the message's original Date header.
Use fixtures sent away from midnight so timezone differences do not obscure
this test.

```r
fixture_day <- as.Date("2026-10-16")  # Replace with the actual fixture date.
same_day <- fetch_mail(imap, subject = subject_for("S01"),
                       since = fixture_day, before = fixture_day)
stopifnot(nrow(same_day) > 0L)
prior_day <- fetch_mail(imap, subject = subject_for("S01"), before = fixture_day - 1)
next_day <- fetch_mail(imap, subject = subject_for("S01"), since = fixture_day + 1)
stopifnot(nrow(prior_day) == 0L, nrow(next_day) == 0L)
```

Expected: the fixture is included on its sent date and excluded by adjacent
ranges. Also compare `Date` values against ISO date strings as inputs.

### F07 Distinguish sent date from mailbox arrival date

Optional: use a controlled fixture whose valid Date header is several days
older than its mailbox insertion date, created with a test mail client/server.

Expected: filtering by the original sent date finds it; filtering only by the
arrival date does not. This checks SENTSINCE/SENTBEFORE semantics.

### F08 Return a typed empty result

```r
none <- fetch_mail(imap, subject = paste0(test_tag, "-does-not-exist"))
stopifnot(nrow(none) == 0L, identical(names(none), names(mail)))
stopifnot(is.logical(none$has_attachment), nrow(attachments(none)) == 0L)
```

Expected: a zero-row tibble, not NULL, a fabricated row, or an error.

### F09 Represent no attachments with an empty table

```r
plain <- fetch_case("S01")
stopifnot(nrow(plain) > 0L, !any(plain$has_attachment))
stopifnot(all(vapply(plain$attachments, nrow, integer(1)) == 0L))
stopifnot(nrow(attachments(plain)) == 0L)
```

Expected: `has_attachment` is `FALSE` and each attachment entry is a zero-row table
with `part`, `name`, `content_type`, `size`, and `disposition` columns. It is not `NA`.

### F10 Detect a real attachment

```r
one_file <- fetch_case("S05")
stopifnot(nrow(one_file) > 0L, all(one_file$has_attachment))
files <- attachments(one_file)
stopifnot(basename(report) %in% files$name)
files
```

Expected: one row per actual attached file, with its `name`, a real `content_type`
(for example `text/plain`), a numeric `size` and the MIME `part` number. If the mail
client shows a file but this case returns an empty table, record FAIL even if
sending succeeded.

### F11 Detect several attachments and flatten them correctly

```r
two_files <- fetch_case("S06")
stopifnot(nrow(two_files) > 0L)
files <- attachments(two_files)
stopifnot(all(c(basename(report), basename(results_file)) %in% files$name))
stopifnot(identical(names(files), c("msg_id", "folder", "part", "name", "content_type", "size", "disposition")))
stopifnot(all(files$msg_id %in% two_files$msg_id), all(files$folder == "INBOX"))
```

Expected: each message has two file rows unless the provider added another
attachment. Attachment bodies are not downloaded by `fetch_mail()`.

### F12 Decode Unicode subjects and filenames

```r
unicode_mail <- fetch_case("S07")
stopifnot(nrow(unicode_mail) > 0L)
stopifnot(all(grepl("Résumé été", unicode_mail$subject, fixed = TRUE)))
unicode_mail |> select(subject, body)
attachments(unicode_mail)
```

Expected: the received subject, body, and accented filename match the mail client
without escape sequences or garbled characters.

### F13 Filter and rearrange with dplyr

```r
selected <- mail |>
  filter(has_attachment) |>
  arrange(desc(date)) |>
  select(msg_id, folder, date, subject, has_attachment, attachments)
stopifnot(all(selected$has_attachment))
attachments(selected)
```

Expected: selected rows retain the correct attachment tables, message IDs, and
folders after filtering and rearranging. Also try an empty selection.

### F14 Fetch several folders including an empty one

Optional: in your mail client, copy an S01 fixture into `cfg$other_folder` and
create an empty folder named `tidymail-empty`. Use the actual server names.

```r
multi <- fetch_mail(imap, folder = c("INBOX", cfg$other_folder, "tidymail-empty"),
                   subject = subject_for("S01"), include_body = FALSE)
stopifnot(all(c("INBOX", cfg$other_folder) %in% multi$folder))
```

Expected: rows from both nonempty folders bind correctly; the empty folder adds
no rows. The same UID may occur in different folders, so verify identity with
`folder` plus `msg_id` rather than requiring globally unique IDs.

### F15 Deduplicate repeated folder requests

```r
single <- fetch_mail(imap, folder = "INBOX", subject = subject_for("S01"))
repeated <- fetch_mail(imap, folder = c("INBOX", "INBOX"), subject = subject_for("S01"))
stopifnot(nrow(single) == nrow(repeated))
```

Expected: asking for the same folder twice does not duplicate its rows. Avoid
running this while new matching fixtures are arriving.

### F16 Decode common message body formats

Optional: prepare fixtures in another mail client or controlled server:

- A text/plain-only message.
- An HTML-only message.
- A multipart/alternative message with matching plain and HTML versions.
- A multipart/mixed message with a body and attached text file.
- Quoted-printable and base64 text parts.
- A declared Latin-1 message containing `café`.
- Folded Q/B encoded subject and display-name headers.

Expected: body text is readable; multipart alternatives prefer plain text;
attached notes are not mistaken for the body; header names decode correctly.
Record the client and original MIME structure for any failure, excluding secrets.

### F17 Preserve text around malformed HTML entities

```r
send_case("F17", "<p>Keep this text &#55296; and &#1114112;.</p>", use_md = FALSE)
```

After it arrives, retrieve with `fetch_case("F17")`. Expected: readable text is
preserved; invalid entities do not turn the entire body into `NA`. A provider
may sanitize entities before delivery; inspect the raw message when interpreting
this result.

### F18 Preserve mailbox state and recover after errors

Note read/unread flags in your mail client, fetch the fixtures twice, then try
an invalid folder followed by a valid fetch:

```r
check_error(fetch_mail(imap, folder = paste0("missing-", test_tag)))
fetch_case("S01")
```

Expected: the invalid folder error names the problem and provides available
folders where possible; the subsequent valid fetch succeeds. Retrieval does
not mark unread messages as read or modify their contents.

### F19 Download attachments and compare them with the originals

```r
two_files <- fetch_case("S06")
dl_dir <- file.path(out_dir, "f19")
unlink(dl_dir, recursive = TRUE)            # a fresh folder, so no name is already taken
saved <- download_attachments(two_files, imap, dir = dl_dir)
saved
stopifnot(nrow(saved) >= 2L, all(file.exists(saved$path)))
stopifnot(all(dirname(normalizePath(saved$path)) == normalizePath(dl_dir)))   # flat: no subfolders
stopifnot(all(grepl("^INBOX_[0-9]+_", basename(saved$path))))                 # <folder>_<msg_id>_<name>
same <- function(original) {
  got <- saved$path[saved$name == basename(original)]
  length(got) == 1L && unname(tools::md5sum(got)) == unname(tools::md5sum(original))
}
stopifnot(same(report), same(results_file))
stopifnot(all(saved$size == file.size(saved$path)))
```

Expected: the files are written directly in the folder, with no subfolders; their
bytes are identical to the originals sent in S06, and `size` is each saved file's
size. Nothing is opened or run. Running it again must not create copies:

```r
again <- download_attachments(two_files, imap, dir = dl_dir)
stopifnot(identical(again$path, saved$path), length(list.files(dl_dir)) == nrow(saved))

nested <- download_attachments(two_files, imap, dir = file.path(out_dir, "f19-tree"), simplify = FALSE)
stopifnot(all(grepl("UID[0-9]+", nested$path)))                        # one folder per message
```

### F20 Download only the attachments you chose

```r
chosen <- two_files
chosen$attachments <- lapply(chosen$attachments, function(a) a[a$name == basename(report), ])
before <- length(list.files(out_dir, recursive = TRUE))
only_one <- download_attachments(chosen, imap, dir = file.path(out_dir, "chosen"))
stopifnot(nrow(only_one) == nrow(chosen), all(only_one$name == basename(report)))
```

Expected: exactly one file per message is written, the one kept in the `attachments`
column. A table with every row removed downloads nothing for that message and
makes no request for it.

### F21 A large attachment neither slows nor breaks reading

```r
big_file <- file.path(out_dir, "big.bin")
writeBin(as.raw(rep(1:250, length.out = 5e6)), big_file)
build_msg(cfg$email, subject_for("F21"), "Large attachment test", attachments = big_file) |>
  send_mail(smtp)
# once it has arrived:
elapsed <- system.time(big <- fetch_case("F21"))[["elapsed"]]
stopifnot(nrow(big) == 1L, big$has_attachment, grepl("Large attachment test", big$body))
elapsed
attachments(big)$size
```

Expected: the fetch returns in seconds, not minutes, and does not fail with "the
server returned an error". A message says the body reached the 256 KB limit. The
body text is readable, and `size` is about 6.7 million bytes (base64 makes the
transfer larger than the file). Then
`download_attachments(big, imap, file.path(out_dir, "f21"))` saves it as
`INBOX_<msg_id>_big.bin` with the same size and md5 as `big_file`.

### F22 An embedded signature image is not an attachment

Optional: from your mail client, send yourself a message with a logo embedded in
the signature (inline, not attached) and no real attachment.

```r
signed <- fetch_mail(imap, subject = "your signature test subject")
stopifnot(nrow(signed) > 0L, !any(signed$has_attachment))
```

Expected: `has_attachment` is `FALSE` and the logo is not listed. Attach a real file
to a similar message and expect `TRUE` with only that file listed.

## Validation errors

These cases should fail before network access. `check_error()` ensures each
expression actually raises an error. Verify messages identify the relevant
input rather than exposing a cryptic backend failure.

### V01 Warn about invalid email addresses

```r
check_error(smtp_account("gmail", email = "not-an-address"))
check_warning(bad <- build_msg("not-an-address", "Test", "Test"))
check_warning(build_msg(c(cfg$email, NA_character_), "Test", "Test"))
check_warning(build_msg(cfg$email, "Test", "Test", cc = "bad"))
check_warning(build_msg(cfg$email, "Test", "Test", bcc = "bad"))
check_error(build_msg(paste0(cfg$email, "\nBcc: other@example.com"), "Test", "Test"))
bad                                   # the value is marked "(not a valid address)"
check_error(send_mail(bad, smtp))
```

Expected: `smtp_account()` rejects the bad address outright. `build_msg()` does not
fail on a bad address: it warns, naming each one under its field and saying to
inspect the draft with `print(msg)` or `preview_mail(msg)`, and returns the draft.
Printing it marks the bad value, `send_mail()` refuses it and sends nothing, and a
line break in an address is an error straight away. `is_address()` checks syntax
only (the default, so it needs no network); it does not verify that a mailbox
exists.

### V02 Reject invalid connection settings

```r
check_error(smtp_account("custom", cfg$email))
check_error(imap_account("custom", cfg$email, host = "   "))
check_error(smtp_account("gmail", cfg$email, port = 0))
check_error(imap_account("gmail", cfg$email, port = 65536))
check_error(imap_account("gmail", cfg$email, port = 1.5))
check_error(smtp_account("gmail", cfg$email, use_ssl = NA))
check_error(smtp_account("gmail", cfg$email, use_ssl = "TRUE"))
check_error(imap_account("gmail", cfg$email, pass_envvar = ""))
```

Expected: custom hosts, valid ports, logical TLS flags, and usable environment
variable names are required. Invalid TLS flags never silently disable TLS.

### V03 Reject missing passwords without prompting

```r
check_error(register_creds(smtp, password = ""))
check_error(register_creds(smtp, password = NA_character_))
check_error(register_creds(smtp))
check_error(register_creds(smtp, password = c("a", "b")))
check_error(make_smtp(password = "x"))     # constructors have no password argument
check_error(smtp_account("gmail", cfg$email, "user", "x"))   # old positional password
```

Expected: a clear non-empty password error from `register_creds()`, with no
tidymail prompt and no keychain changes. A password given to a constructor is
refused with a message pointing to `register_creds()`, and an old positional
password is refused instead of being taken as `pass_envvar`.

### V04 Reject invalid message content and flags

```r
check_error(build_msg(cfg$email, NA_character_, "Test"))
check_error(build_msg(cfg$email, "Test", NA_character_))
check_error(build_msg(cfg$email, "Bad\r\nInjected: header", "Test"))
check_error(build_msg(cfg$email, "Test", "Test", use_md = NA))
check_error(send_mail(build_msg(cfg$email, "Test", "Test"), smtp, verbose = NA))
```

Expected: errors identify invalid fields; nothing is sent. Explicit empty strings
have different behavior, covered by S13.

### V05 Warn about missing files and directories

```r
check_warning(msg1 <- add_attachment(msg, file.path(out_dir, "missing-file.txt")))
check_warning(msg2 <- add_attachment(msg, out_dir))
check_warning(msg3 <- build_msg(cfg$email, "Test", "Test", attachments = file.path(out_dir, "missing-file.txt")))
check_error(build_msg(cfg$email, "Test", "Test", attachments = NA_character_))
msg3                                  # the attachment is marked "(not found)"
check_error(send_mail(msg3, smtp))
```

Expected: a missing file or a directory is a warning that names it (`not found`,
`a directory`) and points to `print(msg)` / `preview_mail(msg)`. Only the new file
is reported by `add_attachment()`. A non-path is an error. `send_mail()` refuses
the draft and sends nothing. The same refusal happens for a file that existed
when the draft was built and was deleted before sending.

### V06 Reject the old field form, a swapped call, and invalid drafts

```r
check_error(send_mail(smtp, to = cfg$email, subject = "Test", body = "Test"))
check_error(send_mail(smtp, msg))
check_error(send_mail("not-a-draft", smtp))
```

Expected: the first error says message fields now belong to `build_msg()` and shows
the pipe form; the second says the message comes first; the third rejects a value
that is not a draft. Nothing is sent.

### V07 Reject invalid searches

```r
check_error(fetch_mail(imap, folder = character(0)))
check_error(fetch_mail(imap, folder = NA_character_))
check_error(fetch_mail(imap, since = "not-a-date"))
check_error(fetch_mail(imap, since = "2026-10-16", before = "2026-10-01"))
check_error(fetch_mail(imap, from = NA_character_))
check_error(fetch_mail(imap, include_body = NA))
```

Expected: invalid searches fail before connecting.

### V08 Reject invalid preview requests

```r
check_error(preview_mail("not-a-draft"))
check_error(preview_mail(msg, file = ""))
check_error(preview_mail(msg, file = out_dir))
check_error(preview_mail(msg, browse = NA))
```

Expected: helpful errors and no browser opening.

A draft with problems can still be previewed: `preview_mail(msg3, browse = FALSE)`
saves a page whose first row, "Problems", lists them, and the missing file is
left out of the composed message.

### V09 Reject attachment helpers on unrelated data

```r
check_error(attachments(data.frame(subject = "Unrelated data")))
check_error(download_attachments("not-a-mail-table", imap, out_dir))
```

Expected: an explanation of the required mailbox tibble or attachment list-column.

### V10 Address predicates preserve missing values

```r
stopifnot(identical(is_address(c("a@example.com", "not-an-address", NA_character_)),
                    c(TRUE, FALSE, NA)))
check_error(is_address(123))
```

Expected: valid-looking addresses return TRUE, invalid syntax returns FALSE,
and missing input stays NA. By default no network or mailbox verification occurs.
With the optional `pingr` package, `is_address("a@gmail.com", mxlookup = TRUE)` is
`TRUE` and `is_address("a@no-such-domain-tidymail.example", mxlookup = TRUE)` is
`FALSE`; turn the network off and the second stays `FALSE` without an error.

## Optional server and platform tests

### X01 Use a nondefault IMAP port

Use a real test endpoint listening on a nondefault port, and set `port` in
`imap_account()`. Expected: connection uses that port and retrieves fixtures.
Changing the port to a nonexistent endpoint is not a useful positive test.

### X02 Test transport TLS modes

Verify your server's supported configurations, such as SMTP STARTTLS on 587,
SMTP implicit TLS on 465, and IMAP implicit TLS on 993. Record each supported
combination. Expected: each advertised TLS configuration connects securely.
If a supported mode cannot be expressed or fails, record FAIL.
Do not disable TLS to work around a TLS configuration problem with real secrets.

### X03 Reject an invalid server certificate

On a controlled server with a deliberately invalid certificate, try sending or
fetching with TLS enabled using dummy credentials. Expected: certificate
validation fails; the package does not silently bypass verification.

### X04 Handle server rejection and connection loss

Use a controlled server that rejects a recipient or disconnects during the
operation. Expected: errors are reported clearly, partial delivery is not
presented as universal success, and a later new operation works. Do not assume
an error proves nothing was delivered; inspect server/recipient state before
retrying a send. Avoid nonexistent hosts that might hang without a timeout.

### X05 Exercise representative providers and operating systems

Repeat the essential cases C01/C02, D03/D05, S01/S05/S07, and F01/F09/F10 on each
provider and OS you intend to claim as supported. Expected: the essential cases
pass consistently on each advertised combination. Preset hostnames do not imply
that a provider allows password authentication for every account. OAuth-only
accounts are outside this release's scope.

### X06 Retrieve a larger matching mailbox

On a test mailbox with many fixtures, compare returned message counts against
an independent client/server search using the same criteria. Expected: no
truncation or missing IDs. Start with headers only and record elapsed time and
approximate mailbox size. Do not impose an arbitrary speed threshold.

### X07 Run without the optional keyring dependency

In an isolated R library/container without `keyring`, construct accounts and
authenticate through the named environment variables. Expected:
environment-only sending/fetching work; calling `register_creds()` explains how
to install `keyring`.

## Release decision and cleanup

All essential cases must pass for the configurations you advertise. Investigate
FAIL results before releasing. SKIP is acceptable only with a recorded reason
and a corresponding limit on support claims. Keep MIME fixtures and server
responses for failures only after removing private message content and secrets.

Remove test messages and temporary test folders through your mail client when
finished. Delete saved test credentials only if you no longer need them:

```r
# Optional cleanup; these remove the test account's saved credentials.
forget_creds(smtp)
forget_creds(imap)
Sys.unsetenv(c("TIDYMAIL_RELEASE_SMTP_PASSWORD", "TIDYMAIL_RELEASE_IMAP_PASSWORD"))
```

Do not attach raw account objects, passwords, or private email bodies to a
public bug report. Record the case ID, observed behavior, expected behavior,
versions, provider, and operating system instead.
