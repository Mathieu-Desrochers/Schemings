(import srfi-1)

(declare (unit matrix-intern))

(declare (uses exceptions))
(declare (uses hungarian))

;; encapsulates a matrix
(define-typed-record matrix-record
  (vector vector)
  (rows-count fixnum)
  (columns-count fixnum))

;; invokes a procedure with a hungarian cost matrix
(: with-hungarian-cost-matrix* (forall (r) ((struct matrix-record) (pointer -> r) -> r)))
(define (with-hungarian-cost-matrix* matrix procedure)
  (with-guaranteed-release
    (lambda ()
      (malloc-hungarian-cost-matrix
        (matrix-record-rows-count matrix)
        (matrix-record-columns-count matrix)))
    (lambda (hungarian-cost-matrix*)
      (for-each
        (lambda (row)
          (for-each
            (lambda (column)
              (hungarian-cost-matrix-set!
                hungarian-cost-matrix* row column (matrix-ref matrix row column)))
            (iota (matrix-record-columns-count matrix))))
        (iota (matrix-record-rows-count matrix)))
      (procedure hungarian-cost-matrix*))
    (lambda (hungarian-cost-matrix*)
      (free-hungarian-cost-matrix
        hungarian-cost-matrix*
        (matrix-record-rows-count matrix)
        (matrix-record-columns-count matrix)))))

;; invokes a procedure with a hungarian problem
(: with-hungarian-problem-t* (forall (r) (pointer fixnum fixnum (pointer -> r) -> r)))
(define (with-hungarian-problem-t* hungarian-cost-matrix* rows-count columns-count procedure)
  (with-guaranteed-release
    (lambda ()
      (let ((hungarian-problem-t* (malloc-hungarian-problem-t)))
        (hungarian-init hungarian-problem-t* hungarian-cost-matrix* rows-count columns-count 1)
        hungarian-problem-t*))
    procedure
    (lambda (hungarian-problem-t*)
      (hungarian-free hungarian-problem-t*)
      (free-hungarian-problem-t hungarian-problem-t*))))
