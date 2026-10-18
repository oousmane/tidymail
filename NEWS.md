# tidymail 0.0.0.9000

Development version.

## Sending

* `build_msg()` creates a message as plain R data: `to`, `subject` and `body`,
  then named `cc`, `bcc`, `attachments` and `use_md`. A message has no sender;
  `send_mail(msg, account)` sends it from the account's email address.
* `add_attachment()` attaches a file and `preview_mail()` renders a message as
  local HTML without an account, as a viewer page or a saved file.
* An address that fails `is_address()`, or an attachment that is not a readable
  file, produces a warning that names it. `print()` and `preview_mail()` mark the
  problems, and `send_mail()` refuses the message, checking the files again when
  it sends. A control character in an address is an error, because it could
  inject mail headers. Attachments over 20 MB in total produce a note.
* `send_mail()` composes with 'blastula'; its arguments after the account are
  named-only.
* `is_address()` checks address syntax offline. `mxlookup = TRUE` also looks up
  each domain's MX record and needs 'pingr'.

## Reading

* `fetch_mail()` returns messages as a tibble: `msg_id`, `folder`, `date` (UTC),
  decoded `from`, `to`, `cc`, `bcc` and `subject`, `body`, `has_attachment` and
  `attachments`. It searches several folders, filters by `since`, `before`,
  `from` and `subject` (escaped before they reach the server), and keeps the
  newest `n_max` messages per folder. `msg_id` and `folder` together identify a
  message.
* Headers decode RFC 2047 encoded words. A body is the `text/plain` part, or
  the `text/html` part reduced to text, decoded from its transfer encoding and
  charset.
* `body_limit` reads at most the first 256 KB of each body, so a large
  attachment is not downloaded to read a few lines of text. A body that cannot be
  downloaded is `NA` with a warning, and a failing header fetch warns instead of
  falling back silently.
* Attachments are listed from each message's MIME structure and never
  downloaded by `fetch_mail()`. Images embedded in a message do not count.
  `attachments()` flattens the list-column into one row per file.
* `download_attachments()` saves the attachments listed in the rows it is given.
  Files are written to one directory as `<folder>_<msg_id>_<file name>` (or one
  folder per message with `simplify = FALSE`); existing files are overwritten.
  File names are made safe, nothing is opened or run, directories and links are
  never written through, and a message that cannot be downloaded warns without
  stopping the others.

## Accounts and passwords

* `smtp_account()`, `imap_account()` and `mail_account()` describe how to log in.
  Creating one never reads or writes a password, and the object holds none.
* `register_creds()` saves a password in the native system keychain and
  `forget_creds()` removes it. Environment variables named by `pass_envvar` are
  read first. Fallback keyring backends are rejected, and a keychain that exists
  but cannot be read warns.
* SMTP and IMAP are independent logins; a combined `mail_account("custom", ...)`
  needs only the host of the transport it uses.
