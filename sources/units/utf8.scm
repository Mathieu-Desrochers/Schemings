(import srfi-4)
(import srfi-13)

(import (chicken blob))
(import (chicken condition))
(import (chicken format))

(declare (unit utf8))

(declare (uses icu))
(declare (uses utf8-intern))

;; returns a utf8 string length
(: utf8-length (string -> fixnum))
(define (utf8-length string)
  (utf8-with-string->u-char*
    string
    (lambda (u-char*)
      (u-strlen u-char*))))

;; upper cases a utf8 string
(: utf8-upper-case (string -> string))
(define (utf8-upper-case string)
  (let* ((max-upper-length (+ (* (string-length string) 2) 1))
         (upper (make-u8vector max-upper-length 0)))
    (utf8-with-u-case-map*
      (lambda (u-case-map*)
        (utf8-with-u-error-code*
          (lambda (u-error-code*)
            (let ((upper-length
                    (u-case-map-utf8-to-upper
                      u-case-map* upper max-upper-length string (string-length string) u-error-code*)))
              (unless (eq? (u-error-code-value u-error-code*) 0)
                (abort
                  (format "failed to upper case string ~A with error code ~A"
                    string (u-error-code-value utf8-error-code*))))
              (blob->string (u8vector->blob (subu8vector upper 0 upper-length))))))))))

;; lower cases a utf8 string
(: utf8-lower-case (string -> string))
(define (utf8-lower-case string)
  (let* ((max-lower-length (+ (* (string-length string) 2) 1))
         (lower (make-u8vector max-lower-length 0)))
    (utf8-with-u-case-map*
      (lambda (u-case-map*)
        (utf8-with-u-error-code*
          (lambda (u-error-code*)
            (let ((lower-length
                    (u-case-map-utf8-to-lower
                      u-case-map* lower max-lower-length string (string-length string) u-error-code*)))
              (unless (eq? (u-error-code-value u-error-code*) 0)
                  (abort
                    (format "failed to upper case string ~A with error code ~A"
                      string (u-error-code-value utf8-error-code*))))
              (blob->string (u8vector->blob (subu8vector lower 0 lower-length))))))))))

;; removes the accents from a utf8 string
(: utf8-remove-accents (string -> string))
(define (utf8-remove-accents string)
  (utf8-transliterate "NFD; [:Nonspacing Mark:] Remove; NFC" string))

;; returns a utf8 string in a different script
(: utf8-change-script (string string string -> string))
(define (utf8-change-script source-script target-script string)
  (let ((transliterator-id (string-append source-script "-" target-script)))
    (utf8-transliterate transliterator-id string)))
