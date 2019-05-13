(declare (unit date-time))

(declare (uses date-time-intern))
(declare (uses exceptions))
(declare (uses time))

;; encapsulates a date
(define-typed-record date
  (year fixnum)
  (month fixnum)
  (day fixnum))

;; encapsulates a date-time
(define-typed-record date-time
  (year fixnum)
  (month fixnum)
  (day fixnum)
  (hour fixnum)
  (minute fixnum)
  (second fixnum))

;; encapsulates a time
(define-typed-record time
  (hour fixnum)
  (minute fixnum)
  (second fixnum))

;; returns the current UTC date
(: date-today (-> (struct date)))
(define (date-today)
  (with-now-tm*
    (lambda (tm*)
      (make-date
        (+ (tm-year tm*) 1900)
        (+ (tm-mon tm*) 1)
        (tm-mday tm*)))))

;; returns the current UTC date time
(: date-time-now (-> (struct date-time)))
(define (date-time-now)
  (with-now-tm*
    (lambda (tm*)
      (make-date-time
        (+ (tm-year tm*) 1900)
        (+ (tm-mon tm*) 1)
        (tm-mday tm*)
        (tm-hour tm*)
        (tm-min tm*)
        (tm-sec tm*)))))

;; returns the current UTC time
(: time-now (-> (struct time)))
(define (time-now)
  (with-now-tm*
    (lambda (tm*)
      (make-time
        (tm-hour tm*)
        (tm-min tm*)
        (tm-sec tm*)))))

;; adds days to a date
(: date-add ((struct date) fixnum -> (struct date)))
(define (date-add date days)
  (with-tm*
    (- (date-year date) 1900)
    (- (date-month date) 1)
    (+ (date-day date) days)
    0
    0
    0
    (lambda (tm*)
      (make-date
        (+ (tm-year tm*) 1900)
        (+ (tm-mon tm*) 1)
        (tm-mday tm*)))))

;; adds seconds to a date time
(: date-time-add ((struct date-time) fixnum -> (struct date-time)))
(define (date-time-add date-time seconds)
  (with-tm*
    (- (date-time-year date-time) 1900)
    (- (date-time-month date-time) 1)
    (date-time-day date-time)
    (date-time-hour date-time)
    (date-time-minute date-time)
    (+ (date-time-second date-time) seconds)
    (lambda (tm*)
      (make-date-time
        (+ (tm-year tm*) 1900)
        (+ (tm-mon tm*) 1)
        (tm-mday tm*)
        (tm-hour tm*)
        (tm-min tm*)
        (tm-sec tm*)))))

;; adds seconds to a time
(: time-add ((struct time) fixnum -> (struct time)))
(define (time-add time seconds)
  (with-tm*
    70
    0
    1
    (time-hour time)
    (time-minute time)
    (+ (time-second time) seconds)
    (lambda (tm*)
      (make-time
        (tm-hour tm*)
        (tm-min tm*)
        (tm-sec tm*)))))

;; returns the number of days between two dates
(: date-diff ((struct date) (struct date) -> fixnum))
(define (date-diff date-from date-to)
  (inexact->exact
    (/
      (-
        (seconds-since-epoch
          (- (date-year date-to) 1970)
          (- (date-month date-to) 1)
          (date-day date-to)
          0
          0
          0)
        (seconds-since-epoch
          (- (date-year date-from) 1970)
          (- (date-month date-from) 1)
          (date-day date-from)
          0
          0
          0))
      86400)))

;; returns the number of seconds between two date times
(: date-time-diff ((struct date-time) (struct date-time) -> fixnum))
(define (date-time-diff date-time-from date-time-to)
  (inexact->exact
    (-
      (seconds-since-epoch
        (- (date-time-year date-time-to) 1970)
        (- (date-time-month date-time-to) 1)
        (date-time-day date-time-to)
        (date-time-hour date-time-to)
        (date-time-minute date-time-to)
        (date-time-second date-time-to))
      (seconds-since-epoch
        (- (date-time-year date-time-from) 1970)
        (- (date-time-month date-time-from) 1)
        (date-time-day date-time-from)
        (date-time-hour date-time-from)
        (date-time-minute date-time-from)
        (date-time-second date-time-from)))))

;; returns the number of seconds between two times
(: time-diff ((struct time) (struct time) -> fixnum))
(define (time-diff time-from time-to)
  (inexact->exact
    (-
      (seconds-since-epoch
        70
        0
        1
        (time-hour time-to)
        (time-minute time-to)
        (time-second time-to))
      (seconds-since-epoch
        70
        0
        1
        (time-hour time-from)
        (time-minute time-from)
        (time-second time-from)))))

;; parses a date string
(: string->date (string -> (struct date)))
(define (string->date string)
  (let ((date-time (parse-date-time string "%Y-%m-%d")))
    (make-date
      (date-time-year date-time)
      (date-time-month date-time)
      (date-time-day date-time))))

;; parses a date time string
(: string->date-time (string -> (struct date-time)))
(define (string->date-time string)
  (parse-date-time string "%Y-%m-%dT%H:%M:%SZ"))

;; parses a time string
(: string->time* (string -> (struct time)))
(define (string->time* string)
  (let ((date-time (parse-date-time string "%H:%M:%S")))
    (make-time
      (date-time-hour date-time)
      (date-time-minute date-time)
      (date-time-second date-time))))

;; tries to parse a date string
(: try-string->date (string -> (or (struct date) false)))
(define (try-string->date string)
  (with-exception-hiding
    (lambda ()
      (string->date string))))

;; tries to parse a date time string
(: try-string->date-time (string -> (or (struct date-time) false)))
(define (try-string->date-time string)
  (with-exception-hiding
    (lambda ()
      (string->date-time string))))

;; tries to parse a time string
(: try-string->time (string -> (or (struct time) false)))
(define (try-string->time string)
  (with-exception-hiding
    (lambda ()
      (string->time* string))))

;; formats a date
(: date->string ((struct date) -> string))
(define (date->string date)
  (format-date-time
    (make-date-time
      (date-year date)
      (date-month date)
      (date-day date)
      0
      0
      0)
    "%Y-%m-%d"))

;; formats a date time
(: date-time->string ((struct date-time) -> string))
(define (date-time->string date-time)
  (format-date-time
    date-time
    "%Y-%m-%dT%H:%M:%SZ"))

;; formats a time
(: time->string* ((struct time) -> string))
(define (time->string* time)
  (format-date-time
    (make-date-time
      1970
      1
      1
      (time-hour time)
      (time-minute time)
      (time-second time))
    "%H:%M:%S"))
