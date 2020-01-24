(import srfi-13)

(declare (unit i18n))

(declare (uses date-time))

;; returns whether a culture is known
(: i18n-culture-known? (string -> boolean))
(define (i18n-culture-known? culture)
  (or (equal? culture "en")
      (equal? culture "fr")))

;; localizes a string
(: i18n-localize-string (string string string -> string))
(define (i18n-localize-string culture string-en string-fr)
  (cond ((equal? culture "en") string-en)
        ((equal? culture "fr") string-fr)))

;; localizes a day-of-week
(: i18n-day-of-week->string (string fixnum -> string))
(define (i18n-day-of-week->string culture day-of-week)
  (cond
    ((equal? culture "en")
      (cond ((eq? day-of-week 0) "Sunday")
            ((eq? day-of-week 1) "Monday")
            ((eq? day-of-week 2) "Tuesday")
            ((eq? day-of-week 3) "Wednesday")
            ((eq? day-of-week 4) "Thursday")
            ((eq? day-of-week 5) "Friday")
            ((eq? day-of-week 6) "Saturday")))
    ((equal? culture "fr")
      (cond ((eq? day-of-week 0) "Dimanche")
            ((eq? day-of-week 1) "Lundi")
            ((eq? day-of-week 2) "Mardi")
            ((eq? day-of-week 3) "Mercredi")
            ((eq? day-of-week 4) "Jeudi")
            ((eq? day-of-week 5) "Vendredi")
            ((eq? day-of-week 6) "Samedi")))))

;; localizes a month
(: i18n-month->string (string fixnum -> string))
(define (i18n-month->string culture month)
  (cond
    ((equal? culture "en")
     (cond ((equal? month 1) "January")
           ((equal? month 2) "February")
           ((equal? month 3) "March")
           ((equal? month 4) "April")
           ((equal? month 5) "May")
           ((equal? month 6) "June")
           ((equal? month 7) "July")
           ((equal? month 8) "August")
           ((equal? month 9) "September")
           ((equal? month 10) "October")
           ((equal? month 11) "November")
           ((equal? month 12) "December")))
    ((equal? culture "fr")
     (cond ((equal? month 1) "janvier")
           ((equal? month 2) "février")
           ((equal? month 3) "mars")
           ((equal? month 4) "avril")
           ((equal? month 5) "mai")
           ((equal? month 6) "juin")
           ((equal? month 7) "juillet")
           ((equal? month 8) "août")
           ((equal? month 9) "septembre")
           ((equal? month 10) "octobre")
           ((equal? month 11) "novembre")
           ((equal? month 12) "décembre")))))

;; localizes a date to the long format
(: i18n-localize-date-long (string (struct date) -> string))
(define (i18n-localize-date-long culture date)
  (cond
    ((equal? culture "en")
      (string-append
        (i18n-month->string culture (date-month date))
        " "
        (number->string (date-day date))
        ", "
        (number->string (date-year date))))
    ((equal? culture "fr")
      (string-append
        (number->string (date-day date))
        " "
        (i18n-month->string culture (date-month date))
        " "
        (number->string (date-year date))))))

;; localizes an amount of money
(: i18n-localize-money (string fixnum -> string))
(define (i18n-localize-money culture number-of-cents)
  (letrec* ((thousand-separator
              (cond ((equal? culture "en") ",")
                    ((equal? culture "fr") " ")))
            (decimal-separator
              (cond ((equal? culture "en") ".")
                    ((equal? culture "fr") ",")))
            (prefix
              (cond ((equal? culture "en") "$ ")
                    ((equal? culture "fr") "")))
            (suffix
              (cond ((equal? culture "en") "")
                    ((equal? culture "fr") " $")))
            (number-of-cents-string
              (number->string number-of-cents))
            (padded-number-of-cents-string
              (string-append
                (make-string (max (- 3 (string-length number-of-cents-string)) 0) #\0)
                number-of-cents-string))
            (format-thousands
              (lambda (dollars accumulator)
                (if (> (string-length dollars) 3)
                  (format-thousands
                    (string-drop-right dollars 3)
                    (string-append thousand-separator (string-take-right dollars 3) accumulator))
                  (string-append dollars accumulator)))))
    (string-append
      prefix
      (format-thousands (string-drop-right padded-number-of-cents-string 2) "")
      decimal-separator
      (string-take-right padded-number-of-cents-string 2)
      suffix)))
