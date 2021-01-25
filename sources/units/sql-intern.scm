(import srfi-1)

(import (chicken condition))
(import (chicken format))

(declare (unit sql-intern))

(declare (uses date-time))
(declare (uses exceptions))
(declare (uses sqlite3))

;; downgrades a value to a database supported format
(: table-value->sql-value (* symbol -> *))
(define (table-value->sql-value value value-type)
  (cond ((eq? value-type 'boolean) (if value 1 0))
        ((not value) #f)
        ((eq? value-type 'date) (date->string value))
        ((eq? value-type 'date-time) (date-time->string value))
        ((eq? value-type 'time) (time->string* value))
        (else value)))

;; upgrades a value from a database supported format
(: sql-value->table-value (* symbol -> *))
(define (sql-value->table-value value value-type)
  (cond ((eq? value-type 'boolean) (eq? value 1))
        ((not value) #f)
        ((eq? value-type 'date) (or (try-string->date value) 'invalid-value))
        ((eq? value-type 'date-time) (or (try-string->date-time value) 'invalid-value))
        ((eq? value-type 'time) (or (try-string->time value) 'invalid-value))
        (else value)))
