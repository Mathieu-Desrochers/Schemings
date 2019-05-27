(import srfi-69)

(declare (unit hash))

(declare (uses hash-intern))

;; hashes elements by a unique numeric key
(: hash-by-unique-key (forall (e) ((list-of e) (e -> number) (e -> *) -> hash-table)))
(define (hash-by-unique-key elements element-key-procedure element-value-procedure)
  (hash-by-unique-key-intern
    elements
    element-key-procedure
    element-value-procedure
    =
    number-hash))

;; hashes elements by a unique string key
(: hash-by-unique-string-key (forall (e) ((list-of e) (e -> string) (e -> *) -> hash-table)))
(define (hash-by-unique-string-key elements element-key-procedure element-value-procedure)
  (hash-by-unique-key-intern
    elements
    element-key-procedure
    element-value-procedure
    string=?
    string-hash))

;; hashes elements by a shared numeric key
(: hash-by-shared-key (forall (e) ((list-of e) (e -> number) (e -> *) -> hash-table)))
(define (hash-by-shared-key elements element-key-procedure element-value-procedure)
  (hash-by-shared-key-intern
    elements
    element-key-procedure
    element-value-procedure
    =
    number-hash))

;; hashes elements by a shared string key
(: hash-by-shared-string-key (forall (e) ((list-of e) (e -> string) (e -> *) -> hash-table)))
(define (hash-by-shared-string-key elements element-key-procedure element-value-procedure)
  (hash-by-shared-key-intern
    elements
    element-key-procedure
    element-value-procedure
    string=?
    string-hash))
