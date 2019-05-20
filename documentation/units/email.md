email
-----
A record with the following fields.

- from
- to
- subject
- body
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
          "bob@hotmail.com"
          "ALERT: Candies in the kitchen"
          "No kidding.\nFirst come first served.\n\nAlice"
          (list
            (make-email-attachment
              "application/pdf"
              "best-candies-ever.pdf"
              (make-blob 100))))))
    (newline)

Run the following commands.

    $ make
    $ ./main

    Date: Mon, 20 May 2019 16:16:53 -0400
    From: alice@hotmail.com
    To: bob@hotmail.com
    Message-ID: <etPan.5ce30b35.53fb867b.1571@laptop>
    Subject: ALERT: Candies in the kitchen
    MIME-Version: 1.0
    Content-Type: multipart/mixed; boundary="KNXiREDvvJDJvJJ2zU9uuPt9"

    --KNXiREDvvJDJvJJ2zU9uuPt9
    Content-Type: text/plain; charset="utf-8"
    Content-Transfer-Encoding: base64
    Content-Disposition: inline

    Tm8ga2lkZGluZy4KRmlyc3QgY29tZSBmaXJzdCBzZXJ2ZWQuCgpBbGljZQ==

    --KNXiREDvvJDJvJJ2zU9uuPt9
    Content-Type: application/pdf
    Content-Transfer-Encoding: base64
    Content-Disposition: attachment; filename="best-candies-ever.pdf"

    gPYiAAAAAAAAxvL//38AAG/0IgAAAAAAKMby//9/AAAAUCsACAAAAFDG8v//fwAAwMby//9/AAAC
    AAAAAAAAAMDG8v//fwAAQMby//9/AAAUOSQAAAAAAAIAAAAAAAAkMDkkAA==

    --KNXiREDvvJDJvJJ2zU9uuPt9--

powered by
----------
The great libetpan.
