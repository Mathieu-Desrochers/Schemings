(import (chicken condition))
(import (chicken format))

(declare (unit regex))

(declare (uses exceptions))
(declare (uses pcre))
(declare (uses regex-intern))

;; invokes a procedure with a compiled regular expression
(: with-compiled-regex (forall (r) (string ((struct regex) -> r) -> r)))
(define (with-compiled-regex pattern procedure)
  (with-guaranteed-release
    (lambda ()
      (regex-compile pattern))
    procedure
    regex-free))

;; executes a compiled regular expression
(: regex-execute-compiled ((struct regex) string -> (list-of string)))
(define (regex-execute-compiled regex string)
  (let* ((pcre* (regex-pcre* regex))
         (pcre-extra* (regex-pcre-extra* regex)))
    (pcre-exec pcre* pcre-extra* string)))

;; executes a regular expression
(: regex-execute (string string -> (list-of string)))
(define (regex-execute pattern string)
  (with-compiled-regex
    pattern
    (lambda (regex)
      (regex-execute-compiled regex string))))
