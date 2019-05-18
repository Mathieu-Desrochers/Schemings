(import srfi-4)

(declare (unit vector))

;; concatenates a list of vectors
(: vector-concatenate ((list-of u8vector) fixnum -> u8vector))
(define (vector-concatenate vectors length)
  (let ((result (make-u8vector length)))
    (letrec (
        (vector-concatenate-intern
          (lambda (vectors vector-index result-index)
            (if (and (not (null? vectors)) (< result-index length))
              (let ((vector (car vectors)))
                (if (eq? vector-index (u8vector-length vector))
                  (vector-concatenate-intern (cdr vectors) 0 result-index)
                  (begin
                    (u8vector-set! result result-index (u8vector-ref vector vector-index))
                    (vector-concatenate-intern vectors (+ vector-index 1) (+ result-index 1)))))
              result))))
      (vector-concatenate-intern vectors 0 0))))
