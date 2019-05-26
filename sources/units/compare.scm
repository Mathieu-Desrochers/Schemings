(import srfi-1)
(import srfi-69)

(import (chicken sort))

(declare (unit compare))

(declare (uses list))

;; encapsulates compare results
(define-typed-record compare-results
  (added-elements (list-of *))
  (changed-elements (list-of *))
  (unchanged-elements (list-of *))
  (deleted-elements (list-of *)))

;; compares two sets of elements
;; elements are matched according to their id
(: compare-elements
  (forall (e1 e2 k) (
    (list-of e1) (e1 -> k)
    (list-of e2) (e2 -> k)
    (e1 e2 -> boolean)
    (e1 -> *) (e1 e2 -> *) (e1 e2 -> *) (e2 -> *) ->
    (struct compare-results))))
(define
  (compare-elements
    original-elements
    original-element-id-procedure
    current-elements
    current-element-id-procedure
    element-changed?-procedure
    make-added-element-procedure
    make-changed-element-procedure
    make-unchanged-element-procedure
    make-deleted-element-procedure)

  ;; make a hash table for each set of elements
  (let ((original-elements-hash-table (make-hash-table = number-hash))
        (current-elements-hash-table (make-hash-table = number-hash)))

    ;; provide a unique negative id
    ;; for any element that does not have one
    (let ((first-original-elements-negative-id
            (- 0 (length original-elements) (length current-elements)))
          (first-current-elements-negative-id
            (- 0 (length current-elements))))

      ;; adds elements to a hash table
      (letrec*
        ((hash-elements
          (lambda (elements hash-table element-id-procedure next-unique-id)
            (unless (null? elements)
              (let* ((element (car elements))
                     (element-id (element-id-procedure element)))
                (if element-id
                  (hash-table-set! hash-table element-id element)
                  (hash-table-set! hash-table next-unique-id element))
                (hash-elements
                  (cdr elements)
                  hash-table element-id-procedure
                  (+ next-unique-id 1)))))))

        ;; hash the original elements
        (hash-elements
          original-elements
          original-elements-hash-table
          original-element-id-procedure
          first-original-elements-negative-id)

        ;; hash the current elements
        (hash-elements
          current-elements
          current-elements-hash-table
          current-element-id-procedure
          first-current-elements-negative-id)

        ;; get the list of distinct elements id
        (let* ((original-elements-id (hash-table-keys original-elements-hash-table))
               (current-elements-id (hash-table-keys current-elements-hash-table))
               (appended-elements-id (append original-elements-id current-elements-id))
               (elements-id (sort (list-distinct-values appended-elements-id identity) <)))

          ;; compare the hash table elements
          ;; for every element id
          (let* ((compare-element
                  (lambda (element-id)
                    (let ((original-element
                            (hash-table-ref/default original-elements-hash-table element-id #f))
                          (current-element
                            (hash-table-ref/default current-elements-hash-table element-id #f)))
                      (cond ((not original-element)
                              (cons
                                (make-added-element-procedure current-element)
                                'added))
                            ((not current-element)
                              (cons
                                (make-deleted-element-procedure original-element)
                                'deleted))
                            ((element-changed?-procedure original-element current-element)
                              (cons
                                (make-changed-element-procedure original-element current-element)
                                'changed))
                            (else
                              (cons
                                (make-unchanged-element-procedure original-element current-element)
                                'unchanged))))))
                  (compare-results
                    (map compare-element elements-id)))

            ;; filters the compare results
            ;; matching a state
            (let ((filter-compare-results
                    (lambda (status)
                      (map car
                        (filter
                          (lambda (compare-result)
                            (eq? (cdr compare-result) status))
                          compare-results)))))

              ;; make the compare results
              (make-compare-results
                (filter-compare-results 'added)
                (filter-compare-results 'changed)
                (filter-compare-results 'unchanged)
                (filter-compare-results 'deleted)))))))))
