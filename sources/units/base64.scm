(import srfi-4)
(import srfi-13)
(import srfi-14)

(import (chicken blob))
(import (chicken condition))
(import (chicken format))

(declare (unit base64))

(declare (uses b64))
(declare (uses exceptions))

;; encodes bytes to base64
(: base64-encode (blob -> string))
(define (base64-encode blob)
  (with-guaranteed-release
    malloc-base64-encodestate
    (lambda (encodestate)
      (base64-init-encodestate encodestate)
      (let* ((blob-u8vector (blob->u8vector blob))
             (string
                (base64-encode-block
                  blob-u8vector
                  (u8vector-length blob-u8vector)
                  encodestate)))
        (unless string
          (abort
            (format "failed to encode bytes to base64")))
        (string-delete
          char-set:whitespace
          string)))
    free-base64-encodestate))

;; decodes bytes from base64
(: base64-decode (string -> blob))
(define (base64-decode string)
  (with-guaranteed-release
    malloc-base64-decodestate
    (lambda (decodestate)
      (base64-init-decodestate decodestate)
      (let* ((string-u8vector (blob->u8vector (string->blob string)))
             (blob-u8vector (make-u8vector (u8vector-length string-u8vector)))
             (blob-u8vector-length
                (base64-decode-block
                  string-u8vector
                  (u8vector-length blob-u8vector)
                  blob-u8vector
                  decodestate)))
        (u8vector->blob
          (subu8vector blob-u8vector 0 blob-u8vector-length))))
    free-base64-decodestate))
