(import srfi-13)

(import (chicken condition))
(import (chicken format))

(declare (unit utf8-intern))

(declare (uses exceptions))
(declare (uses icu))

;; invokes a procedure with a u-error-code*
(: utf8-with-u-error-code* (forall (r) ((pointer -> r) -> r)))
(define (utf8-with-u-error-code* procedure)
  (with-guaranteed-release
    (lambda ()
      (let ((u-error-code* (malloc-u-error-code)))
        (unless u-error-code*
          (abort "failed to allocate u-error-code"))
        u-error-code*))
    procedure
    free-u-error-code))

;; invokes a procedure with a string converted to u-char*
(: utf8-with-string->u-char* (forall (r) (string (pointer -> r) -> r)))
(define (utf8-with-string->u-char* string procedure)
  (let ((max-u-char-length (+ (string-length string) 1)))
    (with-guaranteed-release
      (lambda ()
        (let ((u-char* (malloc-u-char max-u-char-length)))
          (unless u-char*
            (abort
              (format "failed to allocate u-char of size ~A"
                max-u-char-length)))
          u-char*))
      (lambda (u-char*)
        (utf8-with-u-error-code*
          (lambda (u-error-code*)
            (u-str-from-utf8 u-char* max-u-char-length #f string (string-length string) u-error-code*)
            (unless (eq? (u-error-code-value u-error-code*) 0)
              (abort
                (format "failed to convert string ~A to u-char* of size ~A with error code ~A"
                  string max-u-char-length (u-error-code-value u-error-code*))))
            (procedure u-char*))))
      free-u-char)))

;; converts a u-char* to string
(: utf8-u-char*->string (pointer -> string))
(define (utf8-u-char*->string u-char*)
  (let* ((max-string-size (+ (* (u-strlen u-char*) 2) 1))
         (string (make-u8vector max-string-size 0))
         (string-size (make-u32vector 1 0)))
    (utf8-with-u-error-code*
      (lambda (u-error-code*)
        (u-str-to-utf8 string max-string-size string-size u-char* -1 u-error-code*)
        (unless (eq? (u-error-code-value u-error-code*) 0)
          (abort
            (format "failed to convert u-char* to string with error code ~A"
              (u-error-code-value u-error-code*))))
        (blob->string (u8vector->blob (subu8vector string 0 (u32vector-ref string-size 0))))))))

;; invokes a procedure with a u-case-map*
(: utf8-with-u-case-map* (forall (r) ((pointer -> r) -> r)))
(define (utf8-with-u-case-map* procedure)
  (utf8-with-u-error-code*
    (lambda (u-error-code*)
      (with-guaranteed-release
        (lambda ()
          (let* ((u-case-map* (u-case-map-open #f 0 u-error-code*)))
            (unless (eq? (u-error-code-value u-error-code*) 0)
              (abort
                (format "failed to open u-case-map with error code ~A"
                  (u-error-code-value u-error-code*))))
            u-case-map*))
        procedure
        u-case-map-close))))

;; invokes a procedure with a u-transliterator*
(: utf8-with-u-transliterator* (forall (r) (string (pointer -> r) -> r)))
(define (utf8-with-u-transliterator* transliterator-id procedure)
  (utf8-with-string->u-char*
    transliterator-id
    (lambda (u-char*)
      (utf8-with-u-error-code*
        (lambda (u-error-code*)
          (with-guaranteed-release
            (lambda ()
              (let ((u-transliterator* (u-trans-open-u u-char* -1 0 #f -1 #f u-error-code*)))
                (unless (eq? (u-error-code-value u-error-code*) 0)
                  (abort
                    (format "failed to open u-transliterator ~A with error code ~A"
                      transliterator-id (u-error-code-value u-error-code*))))
                u-transliterator*))
            procedure
            u-trans-close))))))

;; transliterates a string
(: utf8-transliterate (string string -> string))
(define (utf8-transliterate transliterator-id string)
  (utf8-with-u-transliterator*
    transliterator-id
    (lambda (u-transliterator*)
      (utf8-with-string->u-char*
        string
        (lambda (u-char*)
          (utf8-with-u-error-code*
            (lambda (u-error-code*)
              (let ((max-u-char-size (+ (string-length string) 1))
                    (u-char-size (make-u32vector 1 (u-strlen u-char*))))
                (u-trans-trans-u-char u-transliterator* u-char* #f max-u-char-size 0 u-char-size u-error-code*))
              (unless (eq? (u-error-code-value u-error-code*) 0)
                (abort
                  (format "failed to transliterate string ~A to ~A with error code ~A"
                    string transliterator-id (u-error-code-value u-error-code*))))
              (utf8-u-char*->string u-char*))))))))
