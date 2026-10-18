# tidymail

tidymail makes sending and retrieving email simple. `send_mail()` uses
blastula for composition and SMTP delivery. `fetch_mail()` uses mRpostman
to return messages as an ordinary tibble you can work with using dplyr.

## Quick start

These examples assume tidymail is installed. Install `keyring` for password
storage and `dplyr` for the filtering examples if needed:

```r
install.packages(c("keyring", "dplyr"))
```

Use an account that permits password-based SMTP and IMAP access. Replace
all example addresses and passwords with your own. For a first live test,
send to yourself.

**Save the password once:** `register_creds()` stores it in the native system
keychain; building an account never touches it. A literal password can still
remain in your script, console history, notebook, or saved R session. Keep it
out of version control and shared files, and remove saved copies after setup.

```r
library(tidymail)

# First-time setup for an account that supports both SMTP and IMAP.
account <- mail_account("gmail", email = "me@gmail.com")
register_creds(account, password = "your-app-password")

build_msg(
  to = "me@gmail.com",
  subject = "tidymail quick-start test",
  body = "Hello from **tidymail**!"
) |>
  send_mail(account)

# Once the message arrives, retrieve it from your inbox.
mail <- fetch_mail(
  account,
  folder = "INBOX",
  subject = "tidymail quick-start test"
)

mail |> dplyr::select(from, subject, date, body)
```

In later sessions, skip `register_creds()`; the password is already saved:

```r
account <- mail_account("gmail", email = "me@gmail.com")
```

For custom servers or different sending and reading logins, see
[Set up an account](#set-up-an-account). The same `send_mail()` and
`fetch_mail()` examples work with `smtp_account()` and `imap_account()` respectively.

## Sending

A message is described with `build_msg()` and delivered with
`send_mail(msg, account)`: the message first, then the account to send it from.
A message can be printed or previewed before it is sent.

### Send a message

```r
build_msg(
  to = "you@example.com",
  subject = "Hello",
  body = "## Welcome\n\nThis message was sent from **R**."
) |>
  send_mail(account)
```

A message is always sent from the account's email address. The body is Markdown
by default. Sending returns the composed email invisibly on success and
errors on delivery failure. Check your inbox and spam folder after a live test.

### Multiple recipients and attachments

Create two small files so the example is ready to run:

```r
report <- tempfile(fileext = ".txt")
writeLines("Monthly report: everything is on track.", report)

results <- tempfile(fileext = ".csv")
write.csv(data.frame(station = c("A", "B"), rainfall = c(12, 18)),
          results, row.names = FALSE)

build_msg(
  to = c("you@example.com", "colleague@example.com"),
  subject = "Monthly report",
  body = "The report and results are attached.",
  cc = "manager@example.com",
  bcc = "archive@example.com",
  attachments = c(report, results)
) |>
  send_mail(account)
```

Replace the recipients before sending. Attachment paths must point to existing
files. To send as another address, use an account for that address.

### Send HTML

```r
build_msg(
  to = "you@example.com",
  subject = "HTML example",
  body = "<h2>Hello</h2><p>This is an <strong>HTML</strong> message.</p>",
  use_md = FALSE
) |>
  send_mail(account)
```

### Build and inspect a draft

`build_msg()` creates a message without sending it. Its fields are `to`,
`subject` and `body`, then the optional `cc`, `bcc`, `attachments` and `use_md`,
which must be named. A message has no sender: it is sent from the email address
of the account given to `send_mail()`, so the same message can go out through any
account:

```r
msg <- build_msg(
  to = "you@example.com",
  subject = "Monthly report",
  body = "See the attached report."
) |>
  add_attachment(report)

msg  # Print a summary.
preview_mail(msg)  # Render the body, recipients, and attachment names.

# Send only when ready.
send_mail(msg, account)
```

An address that does not look valid, or an attachment that is not a readable
file, produces a warning that names it. `build_msg()` still returns the message,
and `print(msg)` and `preview_mail(msg)` mark the problems. `send_mail()` refuses
a message that has problems, and checks the files again when it sends, so nothing
goes out until the message is rebuilt. A line break in an address is an error,
because it could inject mail headers.

`preview_mail()` opens the local HTML preview in the RStudio Viewer or your
browser during an interactive session. It needs no account and sends no
mail. To save a preview without opening it:

```r
msg |> preview_mail(file = "mail-preview.html", browse = FALSE)
```

The saved HTML includes the message text and recipient addresses. Attachment
names are listed, but the files are not embedded. Mail clients may render the
message differently. The message is returned invisibly, so `preview_mail()` can
sit in the middle of a pipe:

```r
build_msg("you@example.com", "Hello", "Hi!") |>
  preview_mail() |>
  send_mail(account)
```

## Retrieving

### Search an inbox

```r
mail <- fetch_mail(
  account,
  folder = "INBOX",
  since = "2026-10-01",
  before = "2026-10-16"
)

mail |> dplyr::select(msg_id, date, from, subject)
```

`since` and `before` include both endpoint dates and refer to the message's
sent date as interpreted by the IMAP server. Omit either bound to leave that
side unrestricted; omit both to search the whole folder.

To narrow the search on the server:

```r
mail <- fetch_mail(
  account,
  folder = "INBOX",
  from = "colleague@example.com",
  subject = "report"
)
```

`from` and `subject` are substring searches on the stored headers. A shared
sender address will match the shared mailbox, not the individual colleague.

### Retrieve headers without bodies

```r
headers <- fetch_mail(
  account,
  folder = "INBOX",
  since = "2026-10-01",
  include_body = FALSE
)
```

This skips message body retrieval and leaves `body` as `NA`. Header fields
and attachment metadata are still fetched. To read bodies, call
`fetch_mail()` with its default `include_body = TRUE`.

### Limit how much you fetch

By default `fetch_mail()` fetches every match, which on a large folder can take
a long time. Use `n_max` to take only the newest messages in each folder (the
ones with the highest IMAP UIDs); a message tells you how many were left out.

```r
recent <- fetch_mail(account, folder = "INBOX", n_max = 50, include_body = FALSE)
#> Fetching the newest 50 of 8214 matching messages in "INBOX".
```

Results come back oldest first within each folder. Narrowing with `since` or
`from` before reaching for `n_max` keeps the messages you actually want.

A message body includes its attachments, so a message with a large attachment
would be downloaded in full just to read a few lines of text. `fetch_mail()`
reads at most the first 256 KB of each body (`body_limit = 262144`) and reports
how many bodies reached the limit. Attachments are never downloaded by
`fetch_mail()`. Raise `body_limit`, or use `Inf`, to read text that comes after a
very large part. A body that cannot be downloaded is `NA` with a warning, and the
other messages still arrive.

### Filter and arrange with dplyr

After fetching, use ordinary data-frame operations without reconnecting:

```r
recent_reports <- mail |>
  dplyr::filter(grepl("report", subject, ignore.case = TRUE)) |>
  dplyr::arrange(dplyr::desc(date)) |>
  dplyr::select(msg_id, folder, date, from, subject, body)
```

### Search several folders

```r
mail <- fetch_mail(
  account,
  folder = c("INBOX", "Archives"),
  since = "2026-10-01"
)
```

Use folder names that exist on your server. An unknown folder produces an
error listing available folders. Keep `folder` alongside `msg_id`: the same
UID can occur in different folders.

### Find and inspect attachments

`fetch_mail()` never downloads attachments. It tells you which messages have
them, and lists what they are:

```r
with_files <- mail |> dplyr::filter(has_attachment)

with_files |> attachments()   # one row per file: name, type, size
```

`has_attachment` is a plain `TRUE`/`FALSE` column, true when a message carries
at least one real attachment. Images embedded in the message itself, such as a
logo in a signature, do not count. If the attachment structure of a message
cannot be read, its `has_attachment` is `NA` (with a warning), never a quiet
`FALSE`.

The `attachments` list-column holds one table per message. An entry such as
`<tibble [0 x 5]>` means the message has **no attachments**; it is not missing
information. `attachments()` flattens it into one row per file with `msg_id`,
`folder`, `part`, `name`, `content_type`, `size` and `disposition`. `size` is
the number of bytes sent over the wire, so a base64 attachment is about a third
larger than the file you end up with.

### Download attachments

`download_attachments()` saves the attachments listed in the rows you give it,
and only those:

```r
mail |>
  dplyr::filter(has_attachment) |>
  download_attachments(account, dir = "~/attachments")
```

To take only some files, filter the `attachments` column first, for example keep
the PDFs:

```r
pdfs <- mail |> dplyr::filter(has_attachment)
pdfs$attachments <- lapply(pdfs$attachments, \(a) a[grepl("\\.pdf$", a$name), ])
download_attachments(pdfs, account, dir = "~/attachments")
```

It returns a table of the files written, with their `path`.

By default (`simplify = TRUE`) the files go straight into `dir`, each named
`<folder>_<msg_id>_<file name>`:

```
~/attachments
├── INBOX_6211_COURRIER 1_08102026.pdf
├── INBOX_6211_COURRIER 2_08102026.pdf
└── Archives_17_report.pdf
```

A message is identified by its folder and `msg_id`, so two messages can never
produce the same name: nothing is renamed, and downloading again overwrites the
same files with the same content instead of piling up copies. The one exception
is a single message with two attachments of the same name; each of those gets its
MIME part number (`INBOX_6211_image-2.png`, `INBOX_6211_image-3.png`) so neither
is lost. A folder name containing a character a file name cannot hold has it
written as `%XX`: `Archives/2024` becomes `Archives%2F2024`.

Use `simplify = FALSE` to keep one folder per message instead, with the files
under their own names: `dir/<login>/<folder>/UID<msg_id>/<file>`.

Attachments come from whoever sent the message: they are saved and never opened
or run. File names are made safe first (path separators and control characters
replaced, no leading dot so an attachment can never become a hidden `.bashrc`),
and every path is checked to be inside `dir`.

### Understand the result

Every fetch returns an ordinary tibble with these columns:

| Column | Meaning |
| --- | --- |
| `msg_id`, `folder` | Folder-scoped IMAP UID and folder name; together they identify a message for later actions such as `download_attachments()` |
| `date` | Message date in UTC |
| `from`, `to`, `cc`, `bcc` | Decoded address headers |
| `subject` | Decoded subject |
| `body` | Decoded plain text, or HTML reduced to text (at most the first 256 KB of the message; see `body_limit`) |
| `has_attachment` | `TRUE`/`FALSE` for whether the message has a real attachment; `NA` if its structure could not be read |
| `attachments` | A list of tables describing the attachments: `part`, `name`, `content_type`, `size`, `disposition` |

An empty search returns zero rows with the same columns. Headers decode
RFC 2047 encoded words and preserve unencoded text. Unreadable body structures
fall back to raw text. UIDs are not permanent global identifiers across
accounts or mailbox resets.

## Set up an account

For an account that supports both sending and reading:

```r
library(tidymail)

account <- mail_account("gmail", email = "me@gmail.com")

# First-time setup: saves the password in your native system keychain.
register_creds(account, password = "your-app-password")
```

Creating an account has no side effects: it never reads or writes a password, so
it is safe to repeat at the top of every script. Only `register_creds()` writes
to the keychain, and it needs the `keyring` package. tidymail accepts macOS
Keychain, Windows Credential Manager and Linux Secret Service. If no native
store is available, saving fails with an error instead of falling back to file or
environment-variable storage. tidymail never prompts for a password, though the
operating system may ask you to unlock or authorize its store.

**First-time setup:** a literal password passed to `register_creds()` can
remain in your script, notebook, console history, or saved R session.
Keychain storage does not remove those copies. Keep real passwords out of
version control and shared files, and remove saved setup code or history
containing them. The examples use placeholders.

In later sessions, only build the account again:

```r
account <- mail_account("gmail", email = "me@gmail.com")
```

Account objects contain connection settings, never a password. The password is
resolved when sending or fetching: the named environment variable first, then
the system keychain. Creating an account does not connect to a server or need
the keychain.

Provider presets supply server settings; they do not guarantee that your
account permits password authentication. This MVP supports password-based
SMTP/IMAP authentication, including app passwords where accepted. OAuth is
not implemented.

### Sending and reading can use different logins

Use only the transport your provider offers. An SMTP-only provider needs no
IMAP configuration:

```r
smtp <- smtp_account(
  "custom", email = "me@example.com",
  host = "smtp.example.com", port = 587
)

imap <- imap_account(
  "custom", email = "me@example.com", user = "me",
  host = "imap.example.com", port = 993
)

# First-time setup: each login has its own saved password.
register_creds(smtp, password = "your-smtp-password")
register_creds(imap, password = "your-imap-password")
```

The two logins may belong to different providers. Each stores its own
password, keyed by transport and `user@host`. Hosts and ports are explicit
for custom services; TLS is enabled by default. `mail_account("custom", ...)`
also accepts just `smtp_host` or just `imap_host`. Only the transport being
used must be configured.

### Automated jobs

You can supply passwords through your job's environment instead of a
keychain. No persistent store is required when using this method:

| Account | Default environment variable |
| --- | --- |
| `smtp_account("gmail", ...)` | `TIDYMAIL_GMAIL_SMTP_PASSWORD` |
| `imap_account("gmail", ...)` | `TIDYMAIL_GMAIL_IMAP_PASSWORD` |
| `mail_account("gmail", ...)` | `TIDYMAIL_GMAIL_PASSWORD` |

Other providers follow the same naming pattern; combined custom accounts
use `TIDYMAIL_PASSWORD`. Set `pass_envvar` to override the name. Environment
values take precedence over saved keychain entries and are never saved
automatically.

To replace a saved password, register it again; to remove it:

```r
register_creds(account, password = "replacement-app-password")
forget_creds(account)
```

## Release testing

Work through the [manual release checklist](docs/release-checklist.md) and
record PASS, FAIL, or SKIP in [the results sheet](docs/release-results.csv).
It covers credential storage, live delivery, previews, retrieval, attachment
metadata, validation, and optional server/platform checks.

## Development status

This is an early development package. The tests cover sending and previewing,
the credential lifecycle (through an isolated test store), connection settings,
mailbox assembly, decoding, and attachment downloads against replayed server
replies. They never send real mail or access your native keychain.

Verified on live servers: sending through a custom SMTP server and reading from
a Dovecot IMAP server (earlier iterations). Not yet verified on a live server:
multi-folder fetching, body decoding with `body_limit`, and attachment
downloads. Mailbox changes, bulk personalized sending and OAuth are outside the
scope of this version.
