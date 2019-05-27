(declare (unit math))

(declare (uses date-time))

;; rounds a value to a number of decimals
(: math-round (number fixnum -> number))
(define (math-round value decimals-count)
  (let* ((multiplicator (expt 10 decimals-count))
         (multiplied-value (* value multiplicator))
         (rounded-multiplied-value (round multiplied-value))
         (rounded-value (/ rounded-multiplied-value multiplicator)))
    (exact->inexact rounded-value)))

;; adds a new value to a cumulative moving average
(: math-cumulative-moving-average-add-value (number fixnum number -> number))
(define (math-cumulative-moving-average-add-value average n value)
  (if (eq? n 0)
    value
    (/ (+ (* average n) value) (+ n 1))))

;; removes a value from a cumulative moving average
(: math-cumulative-moving-average-remove-value (number fixnum number -> number))
(define (math-cumulative-moving-average-remove-value average n value)
  (if (eq? n 1)
    #f
    (/ (- (* average n) value) (- n 1))))

;; returns the number of years between two dates
(: math-calculate-age ((struct date) (struct date) -> number))
(define (math-calculate-age date-from date-to)
  (let* ((months-from (+ (* (date-year date-from) 12) (date-month date-from)))
         (months-to (+ (* (date-year date-to) 12) (date-month date-to)))
         (months
            (if (>= (date-day date-to) (date-day date-from))
              (- months-to months-from)
              (- months-to months-from 1)))
         (days
            (if (>= (date-day date-to) (date-day date-from))
              (- (date-day date-to) (date-day date-from))
              (+ (- 31 (date-day date-from)) (date-day date-to)))))
    (math-round
      (+ (/ months 12)
         (/ days (* 31 12)))
      2)))
