email
-----
A record with the following fields.

- from
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

    Date: Wed, 22 Apr 2020 20:30:02 -0400
    From: alice@hotmail.com
    To: bob@hotmail.com
    Message-ID: <etPan.5ea0e18a.53fb867b.e224@laptop>
    Subject: ALERT: Candies in the kitchen
    MIME-Version: 1.0
    Content-Type: multipart/mixed; boundary="/6KhbH9zeCAQcoMtYZeKqGja"

    --/6KhbH9zeCAQcoMtYZeKqGja
    Content-Type: multipart/alternative; boundary="sAHdHfp525SKo0MEISMqsoR5"

    --sAHdHfp525SKo0MEISMqsoR5
    Content-Type: text/plain; charset="utf-8"
    Content-Transfer-Encoding: base64
    Content-Disposition: inline

    Tm8ga2lkZGluZy4KRmlyc3QgY29tZSBmaXJzdCBzZXJ2ZWQuCgpBbGljZQ==

    --sAHdHfp525SKo0MEISMqsoR5
    Content-Type: text/html; charset="utf-8"
    Content-Transfer-Encoding: base64
    Content-Disposition: inline

    PGh0bWw+PGJvZHk+V2UgaGF2ZSA8Yj5jYW5kaWVzPC9iPiBpbiB0aGUga2l0Y2hlbi48L2JvZHk+
    PC9odG1sPg==

    --sAHdHfp525SKo0MEISMqsoR5--

    --/6KhbH9zeCAQcoMtYZeKqGja
    Content-Type: application/pdf
    Content-Transfer-Encoding: base64
    Content-Disposition: attachment; filename="best-candies-ever.pdf"

    QGX6//9/AABAY/r//38AAIhk+v//fwAAAGX6//9/AAApAAAAAAAAALAAAAAAAAAAeGf6//9/AAAA
    Zfr//38AAEBl+v//fwAA0GT6//9/AABLAy4BCAAAAAIAAAAAAAADKwAAAA==

    --/6KhbH9zeCAQcoMtYZeKqGja--

powered by
----------
The great libetpan.
