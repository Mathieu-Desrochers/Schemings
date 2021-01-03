email
-----
A record with the following fields.

- from
- reply-to
- to
- subject
- body-text
- body-html
- attachments

email-attachment
----------------
A record with the following fields.

- content-type
- file-name
- content

email->string
-------------
Converts an email to string.

__email__  
An email.

__result__  
The encoded email.

try it
------
Place the following code in sources/main.scm.

    (import (chicken blob))

    (declare (uses email))

    (display
      (email->string
        (make-email
          "alice@hotmail.com"
          "complaints-department@hotmail.com"
          "bob@hotmail.com"
          "ALERT: Candies in the kitchen"
          "No kidding.\nFirst come first served.\n\nAlice"
          "<html><body>We have <b>candies</b> in the kitchen.</body></html>"
          (list
            (make-email-attachment
              "application/pdf"
              "best-candies-ever.pdf"
              (make-blob 100))))))
    (newline)

Run the following commands.

    $ make
    $ ./main

    Date: Sun, 3 Jan 2021 14:31:13 -0500
    From: alice@hotmail.com
    Reply-To: complaints-department@hotmail.com
    To: bob@hotmail.com
    Message-ID: <etPan.5ff21b81.53fb867b.14abf@laptop>
    Subject: ALERT: Candies in the kitchen
    MIME-Version: 1.0
    Content-Type: multipart/mixed; boundary="ZAkXV1piAVCVytiiVkulG0CZ"

    --ZAkXV1piAVCVytiiVkulG0CZ
    Content-Type: multipart/alternative; boundary="gQnW4IruWuz2/gaLGmE7lGl6"

    --gQnW4IruWuz2/gaLGmE7lGl6
    Content-Type: text/plain; charset="utf-8"
    Content-Transfer-Encoding: base64
    Content-Disposition: inline

    Tm8ga2lkZGluZy4KRmlyc3QgY29tZSBmaXJzdCBzZXJ2ZWQuCgpBbGljZQ==

    --gQnW4IruWuz2/gaLGmE7lGl6
    Content-Type: text/html; charset="utf-8"
    Content-Transfer-Encoding: base64
    Content-Disposition: inline

    PGh0bWw+PGJvZHk+V2UgaGF2ZSA8Yj5jYW5kaWVzPC9iPiBpbiB0aGUga2l0Y2hlbi48L2JvZHk+
    PC9odG1sPg==

    --gQnW4IruWuz2/gaLGmE7lGl6--

    --ZAkXV1piAVCVytiiVkulG0CZ
    Content-Type: application/pdf
    Content-Transfer-Encoding: base64
    Content-Disposition: attachment; filename="best-candies-ever.pdf"

    BqU1AQgAAADgiv///38AAFBq+v//fwAAwFf6//9/AAAoi////38AADBW+v//fwAAAAAAAAAAAAAD
    AAAAAAAAJGCmNQEIAAAAUK1YAQgAAAAgqVgBCAAAAOBkNQEIAAAAAFf6/w==

    --ZAkXV1piAVCVytiiVkulG0CZ--

powered by
----------
The great libetpan.
