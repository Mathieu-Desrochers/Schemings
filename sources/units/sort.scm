(import (chicken sort))
(import (chicken string))

(declare (unit sort))

;; sorts a list of elements by their numeric value
(: sort-by-number (forall (e) ((list-of e) (e -> number) -> (list-of e))))
(define (sort-by-number elements element-sort-value-procedure)
  (sort
    elements
    (lambda (x y)
      (< (element-sort-value-procedure x)
         (element-sort-value-procedure y)))))

;; sorts a list of elements by their descending numeric value
(: sort-by-number-descending (forall (e) ((list-of e) (e -> number) -> (list-of e))))
(define (sort-by-number-descending elements element-sort-value-procedure)
  (sort
    elements
    (lambda (x y)
      (> (element-sort-value-procedure x)
         (element-sort-value-procedure y)))))

;; sorts a list of elements by their string value
(: sort-by-string (forall (e) ((list-of e) (e -> string) -> (list-of e))))
(define (sort-by-string elements element-sort-value-procedure)
  (sort
    elements
    (lambda (x y)
      (<
        (string-compare3
          (element-sort-value-procedure x)
          (element-sort-value-procedure y))
        1))))

;; sorts a list of elements by their descending string value
(: sort-by-string-descending (forall (e) ((list-of e) (e -> string) -> (list-of e))))
(define (sort-by-string-descending elements element-sort-value-procedure)
  (sort
    elements
    (lambda (x y)
      (>
        (string-compare3
          (element-sort-value-procedure x)
          (element-sort-value-procedure y))
        1))))
