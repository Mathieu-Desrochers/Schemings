(import srfi-69)

(declare (unit hash-intern))

;; hashes elements by a unique key
(: hash-by-unique-key-intern
  (forall (k e) (
    (list-of e) (e -> k) (e -> *) (k k -> boolean) (k -> *) -> hash-table)))
(define (hash-by-unique-key-intern
          elements
          element-key-procedure
          element-value-procedure
          test-procedure
          hash-procedure)

  ;; make the hash table
  (let ((hash-table (make-hash-table test-procedure hash-procedure)))

    ;; hash the elements
    (for-each
      (lambda (element)
        (hash-table-set!
          hash-table
          (element-key-procedure element)
          (element-value-procedure element)))
      elements)

    hash-table))

;; hashes elements by a shared key
(: hash-by-shared-key-intern
  (forall (k e) (
    (list-of e) (e -> k) (e -> *) (k k -> boolean) (k -> *) -> hash-table)))
(define (hash-by-shared-key-intern
          elements
          element-key-procedure
          element-value-procedure
          test-procedure
          hash-procedure)

  ;; make the hash table
  (let ((hash-table (make-hash-table test-procedure hash-procedure)))

    ;; hash the elements
    (for-each
      (lambda (element)
        (hash-table-update!/default
          hash-table
          (element-key-procedure element)
          (lambda (elements-list)
            (cons
              (element-value-procedure element)
              elements-list))
          (list)))
      elements)

    ;; reverse the hashed elements
    ;; into their original order
    (for-each
      (lambda (key)
        (let ((elements-list (hash-table-ref hash-table key)))
          (hash-table-set!
            hash-table
            key
            (reverse elements-list))))
      (hash-table-keys
        hash-table))

    hash-table))
