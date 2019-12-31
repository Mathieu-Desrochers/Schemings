(import  srfi-1)
(import  srfi-69)

(import (chicken sort))

(declare (unit list))

(declare (uses list-intern))

;; returns the index of the elements
;; that appear more than once in a list
;; ignores the false element values
(: list-duplicates-index (forall (e) ((list-of e) (e -> *) -> (list-of fixnum))))
(define (list-duplicates-index
          elements
          element-value-procedure)

  ;; get the elements value
  (let ((elements-value (map element-value-procedure elements))
        (elements-value-count-hash-table
          (make-hash-table
            equal?
            equal?-hash)))

    ;; count the elements value
    (for-each
      (lambda (element-value)
        (when element-value
          (hash-table-update!
            elements-value-count-hash-table
            element-value
            (lambda (element-value-count) (+ element-value-count 1))
            (lambda () 0))))
      elements-value)

    ;; return the index of the elements value
    ;; that were counted more than once
    (filter-map
      (lambda (element-value-with-index)
        (let ((element-value (car element-value-with-index))
              (element-index (cadr element-value-with-index)))
          (if element-value
            (if (> (hash-table-ref elements-value-count-hash-table element-value) 1)
              element-index
              #f)
            #f)))

      ;; zip the first elements value
      ;; with their index
      (zip elements-value (iota (length elements-value))))))

;; returns the index of the elements in a first list
;; whose value can be matched in a second list
;; ignores the false element values
(: list-matches-index
  (forall (e1 e2 v) ((list-of e1) (e1 -> v) (list-of e2) (e2 -> v) -> (list-of fixnum))))
(define (list-matches-index
          first-elements
          first-element-value-procedure
          second-elements
          second-element-value-procedure)

  (list-matches-or-non-matches-index-intern
    first-elements
    first-element-value-procedure
    second-elements
    second-element-value-procedure
    #t
    #f))

;; returns the index of the elements in a first list
;; whose value cannot be matched in a second list
;; ignores the false element values
(: list-non-matches-index
  (forall (e1 e2 v) ((list-of e1) (e1 -> v) (list-of e2) (e2 -> v) -> (list-of fixnum))))
(define (list-non-matches-index
          first-elements
          first-element-value-procedure
          second-elements
          second-element-value-procedure)

  (list-matches-or-non-matches-index-intern
    first-elements
    first-element-value-procedure
    second-elements
    second-element-value-procedure
    #f
    #t))

;; returns the distinct values from a list
;; ignores the false element values
(: list-distinct-values (forall (e v) ((list-of e) (e -> v) -> (list-of v))))
(define (list-distinct-values
          elements
          element-value-procedure)

  ;; build a hash table of the known values
  (let ((known-values-hash-table
          (make-hash-table
            equal?
            equal?-hash)))

    ;; iterate over the elements
    (letrec* (
      (list-distinct-values-iter
        (lambda (remaining accumulated)
          (if (not (null? remaining))

            ;; skip known values
            (let ((value (element-value-procedure (car remaining))))
              (if (or (not value) (hash-table-ref/default known-values-hash-table value #f))
                (list-distinct-values-iter (cdr remaining) accumulated)

                ;; accumulate the unknown values
                (begin
                  (hash-table-set! known-values-hash-table value #t)
                  (list-distinct-values-iter
                    (cdr remaining)
                    (cons value accumulated)))))

            ;; preserve the original order
            (reverse accumulated)))))

      (list-distinct-values-iter elements (list)))))

;; returns the index of the elements
;; whose value matches a filter
(: list-filtered-index
  (forall (e v) ((list-of e) (e -> v) (v -> boolean) -> (list-of fixnum))))
(define (list-filtered-index
          elements
          element-value-procedure
          filter-procedure)

  ;; iterate over the elements
  (letrec (
    (list-filtered-index-iter
      (lambda (remaining accumulated index)
        (if (not (null? remaining))

          ;; accumulate the filtered element indexes
          (if (filter-procedure (element-value-procedure (car remaining)))
            (list-filtered-index-iter
              (cdr remaining)
              (cons index accumulated)
              (+ index 1))
            (list-filtered-index-iter
              (cdr remaining)
              accumulated
              (+ index 1)))

          ;; preserve the original order
          (reverse accumulated)))))

      (list-filtered-index-iter elements (list) 0)))

;; returns whether the value of the elements
;; form a sequence starting from one
(: list-is-sequential (forall (e) ((list-of e) (e -> fixnum) -> boolean)))
(define (list-is-sequential
          elements
          element-value-procedure)
  (equal?
    (sort (map element-value-procedure elements) <)
    (iota (length elements) 1)))

;; invokes a procedure with each element of a list
;; along with its index
(: list-for-each-with-index (forall (e) ((list-of e) (e fixnum -> noreturn) -> noreturn)))
(define (list-for-each-with-index
          elements
          procedure)
  (letrec
      ((list-for-each-with-index-inner
        (lambda (elements index)
          (if (not (null? elements))
            (begin
              (procedure (car elements) index)
              (list-for-each-with-index-inner (cdr elements) (+ index 1)))))))
    (list-for-each-with-index-inner elements 0)))

;; splits a list in sublists
(: list-split (forall (e) ((list-of e) fixnum -> (list-of (list-of e)))))
(define (list-split
          elements
          sublist-size)
  (letrec
      ((list-break-inner
        (lambda (elements accumulated)
          (cond
            ((null? elements) accumulated)
            ((<= (length elements) sublist-size) (cons elements accumulated))
            (else
              (list-break-inner
                (drop elements sublist-size)
                (cons (take elements sublist-size) accumulated)))))))
    (reverse (list-break-inner elements (list)))))
