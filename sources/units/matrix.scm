(import srfi-1)
(import srfi-13)

(declare (unit matrix))

(declare (uses debug))
(declare (uses matrix-intern))

;; makes a matrix
(: make-matrix (fixnum fixnum -> (struct matrix-record)))
(define (make-matrix rows-count columns-count)
  (make-matrix-record
    (make-vector (* rows-count columns-count))
    rows-count columns-count))

;; gets a matrix value
(: matrix-ref ((struct matrix-record) fixnum fixnum -> *))
(define (matrix-ref matrix row column)
  (vector-ref
    (matrix-record-vector matrix)
    (+ (* row (matrix-record-columns-count matrix)) column)))

;; sets a matrix value
(: matrix-set! ((struct matrix-record) fixnum fixnum * -> noreturn))
(define (matrix-set! matrix row column value)
  (vector-set!
    (matrix-record-vector matrix)
    (+ (* row (matrix-record-columns-count matrix)) column) value))

;; returns the number of rows in a matrix
(: matrix-rows-count ((struct matrix-record) -> fixnum))
(define (matrix-rows-count matrix)
  (matrix-record-rows-count matrix))

;; returns the number of columns in a matrix
(: matrix-columns-count ((struct matrix-record) -> fixnum))
(define (matrix-columns-count matrix)
  (matrix-record-columns-count matrix))

;; solves the maximum assignment problem using the hungarian method
(: matrix-maximum-assignment ((struct matrix-record) -> (list-of (pair fixnum fixnum))))
(define (matrix-maximum-assignment matrix)
  (with-hungarian-cost-matrix* matrix
    (lambda (hungarian-cost-matrix*)
      (with-hungarian-problem-t* hungarian-cost-matrix*
        (matrix-rows-count matrix) (matrix-columns-count matrix)
        (lambda (hungarian-problem-t*)
          (hungarian-solve hungarian-problem-t*)
          (filter
            (lambda (row-assignment)
              (< (cdr row-assignment) (matrix-columns-count matrix)))
            (map
              (lambda (row)
                (cons row
                  (hungarian-get-row-assignment hungarian-problem-t*
                    row (matrix-columns-count matrix))))
              (iota (matrix-rows-count matrix)))))))))

;; returns the string representation of a matrix
(: matrix->string ((struct matrix-record) -> string))
(define (matrix->string matrix)
  (string-join
    (map
      (lambda (row)
        (string-append
          "[ "
          (string-join
            (map
              (lambda (column) (number->string (matrix-ref matrix row column)))
              (iota (matrix-columns-count matrix)))
            ", ")
          " ]"))
      (iota (matrix-rows-count matrix)))
    "\n"))
