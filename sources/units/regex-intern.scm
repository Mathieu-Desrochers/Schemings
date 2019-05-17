(import (chicken condition))
(import (chicken format))

(declare (unit regex-intern))

(declare (uses exceptions))
(declare (uses pcre))

;; encapsulates a regular expression
(define-typed-record regex
  (pcre* pointer)
  (pcre-extra* pointer))

;; compiles a regular expression
(: regex-compile (string -> (struct regex)))
(define (regex-compile pattern)
  (let ((pcre* (pcre-compile pattern)))
    (unless pcre*
      (abort
        (format "failed to compile regex with pattern ~A"
          pattern)))
    (let ((pcre-extra* (pcre-study pcre*)))
      (unless pcre-extra*
        (begin
          (pcre-free pcre*)
          (abort
            (format "failed to study regex with pattern ~A"
              pattern))))
      (make-regex
        pcre*
        pcre-extra*))))

;; frees a regular expression
(: regex-free ((struct regex) -> noreturn))
(define (regex-free regex)
  (let ((pcre* (regex-pcre* regex))
        (pcre-extra* (regex-pcre-extra* regex)))
    (pcre-free-study pcre-extra*)
    (pcre-free pcre*)))
