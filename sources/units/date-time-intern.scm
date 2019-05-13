(import (chicken condition))
(import (chicken format))

(declare (unit date-time-intern))

(declare (uses exceptions))
(declare (uses time))

;; normalizes a time structure
;; would replace 90 seconds by 1 minutes and 30 seconds
(: date-time-normalize (pointer -> noreturn))
(define (date-time-normalize tm*)
  (let ((seconds-since-epoch (timegm tm*)))
    (unless (gmtime-r seconds-since-epoch tm*)
      (abort
        (format
          "failed to break down seconds since epoch ~A"
          seconds-since-epoch)))))

;; invokes a procedure with a time structure
;; containing the current UTC date time
(: with-now-tm* (forall (r) ((pointer -> r) -> r)))
(define (with-now-tm* procedure)
  (with-guaranteed-release
    malloc-tm
    (lambda (tm*)
      (unless tm*
        (abort "failed to allocate tm"))
      (let ((seconds-since-epoch (time* #f)))
        (unless (gmtime-r seconds-since-epoch tm*)
          (abort
            (format
              "failed to break down seconds since epoch ~A"
              seconds-since-epoch)))
        (procedure tm*)))
    free-tm))

;; invokes a procedure with a time structure
;; containing a date time
(: with-tm* (forall (r) (fixnum fixnum fixnum fixnum fixnum fixnum (pointer -> r) -> r)))
(define (with-tm* year month day hour minute second procedure)
  (with-guaranteed-release
    malloc-tm
    (lambda (tm*)
      (unless tm*
        (abort "failed to allocate tm"))
      (tm-year-set! tm* year)
      (tm-mon-set! tm* month)
      (tm-mday-set! tm* day)
      (tm-hour-set! tm* hour)
      (tm-min-set! tm* minute)
      (tm-sec-set! tm* second)
      (date-time-normalize tm*)
      (procedure tm*))
    free-tm))

;; returns the number of seconds since the epoch of a date time
(: seconds-since-epoch (fixnum fixnum fixnum fixnum fixnum fixnum -> fixnum))
(define (seconds-since-epoch year month day hour minute second)
  (with-guaranteed-release
    malloc-tm
    (lambda (tm*)
      (unless tm*
        (abort "failed to allocate tm"))
      (tm-year-set! tm* year)
      (tm-mon-set! tm* month)
      (tm-mday-set! tm* day)
      (tm-hour-set! tm* hour)
      (tm-min-set! tm* minute)
      (tm-sec-set! tm* second)
      (date-time-normalize tm*)
      (timegm tm*))
    free-tm))

;; parses a date time
(: parse-date-time (string string -> (struct date-time)))
(define (parse-date-time string date-time-format)
  (with-guaranteed-release
    malloc-tm
    (lambda (tm*)
      (unless tm*
        (abort "failed to allocate tm"))
      (let ((strptime-result (strptime string date-time-format tm*)))
        (unless strptime-result
          (abort
            (format "failed to parse date time ~A with format ~A"
              string
              date-time-format)))
        (make-date-time
          (+ (tm-year tm*) 1900)
          (+ (tm-mon tm*) 1)
          (tm-mday tm*)
          (tm-hour tm*)
          (tm-min tm*)
          (tm-sec tm*))))
    free-tm))

;; formats a date time
(: format-date-time ((struct date-time) string -> string))
(define (format-date-time date-time date-time-format)
  (with-guaranteed-release
    malloc-tm
    (lambda (tm*)
      (unless tm*
        (abort "failed to allocate tm"))
      (tm-year-set! tm* (- (date-time-year date-time) 1900))
      (tm-mon-set! tm* (- (date-time-month date-time) 1))
      (tm-mday-set! tm* (date-time-day date-time))
      (tm-hour-set! tm* (date-time-hour date-time))
      (tm-min-set! tm* (date-time-minute date-time))
      (tm-sec-set! tm* (date-time-second date-time))
      (let ((strftime-result (strftime date-time-format tm*)))
        (unless strftime-result
          (abort
            (format "failed to format date time with format ~A"
              date-time-format)))
        strftime-result))
    free-tm))
