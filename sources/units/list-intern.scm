(import srfi-1)
(import srfi-69)

(import (chicken sort))

(declare (unit list-intern))

;; returns the index of the elements in a first list
;; whose value can or cannot be matched in a second list
;; ignores the false element values
(: list-matches-or-non-matches-index-intern
  (forall (e1 e2 v) (
    (list-of e1) (e1 -> v) (list-of e2) (e2 -> v) boolean boolean -> (list-of fixnum))))
(define (list-matches-or-non-matches-index-intern
          first-elements
          first-element-value-procedure
          second-elements
          second-element-value-procedure
          keep-matches-index
          keep-non-matches-index)

  ;; get the elements value
  (let ((first-elements-value (map first-element-value-procedure first-elements))
        (second-elements-value (map second-element-value-procedure second-elements)))

    ;; hash the second elements value
    (let ((second-elements-value-hash-table
            (make-hash-table
              equal?
              equal?-hash)))
      (for-each
        (lambda (second-element-value)
          (when second-element-value
            (hash-table-set!
              second-elements-value-hash-table
              second-element-value
              #t)))
        second-elements-value)

      ;; sort the returned indexes
      (sort

        ;; search the first elements value
        ;; among the second ones
        (filter-map
          (lambda (first-element-value-with-index)
            (let ((first-element-value (car first-element-value-with-index))
                  (first-element-index (cadr first-element-value-with-index)))
              (if first-element-value
                (if (hash-table-ref/default second-elements-value-hash-table first-element-value #f)
                  (if keep-matches-index first-element-index #f)
                  (if keep-non-matches-index first-element-index #f))
                #f)))

          ;; zip the first elements value
          ;; with their index
          (zip first-elements-value (iota (length first-elements-value))))

        <))))
