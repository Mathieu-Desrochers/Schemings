(import (chicken condition))
(import (chicken format))

(declare (unit json-intern))

(declare (uses date-time))
(declare (uses jansson))

;; makes a value json-t*
(: make-value-json-t* (* -> pointer))
(define (make-value-json-t* value)
  (let ((json-t*
          (cond ((and (integer? value) (exact? value)) (json-integer value))
                ((number? value) (json-real value))
                ((string? value) (json-string value))
                ((boolean? value) (json-boolean (if value 1 0)))
                ((eq? value 'json-null) (json-null))
                (else #f))))
    (unless json-t*
      (abort
        (format "failed to make a value json-t* for value ~A"
          value)))
    json-t*))

;; makes an object json-t*
(: make-object-json-t* (-> pointer))
(define (make-object-json-t*)
  (let ((json-t* (json-object)))
    (unless json-t*
      (abort "failed to make an object json-t*"))
    json-t*))

;; makes an array json-t*
(: make-array-json-t* (-> pointer))
(define (make-array-json-t*)
  (let ((json-t* (json-array)))
    (unless json-t*
      (abort "failed to make an array json-t*"))
    json-t*))

;; returns the value contained in a json-t*
(: get-value-from-json-t* (pointer -> *))
(define (get-value-from-json-t* json-t*)
  (let* ((json-type (json-typeof json-t*))
         (value
            (cond ((eq? json-type json-type-integer) (json-integer-value json-t*))
                  ((eq? json-type json-type-real) (json-real-value json-t*))
                  ((eq? json-type json-type-string) (json-string-value json-t*))
                  ((eq? json-type json-type-true) #t)
                  ((eq? json-type json-type-false) #f)
                  ((eq? json-type json-type-null) #f)
                  (else 'unknown-json-type))))
    (if (eq? value 'unknown-json-type)
      (abort "failed to get value from a json-t*"))
    value))

;; downgrades a value to a json supported format
(: field-value->json-value (* symbol -> *))
(define (field-value->json-value value value-type)
  (cond ((and (not value) (not (eq? value-type 'boolean))) 'json-null)
        ((eq? value-type 'date) (date->string value))
        ((eq? value-type 'date-time) (date-time->string value))
        ((eq? value-type 'time) (time->string* value))
        (else value)))

;; upgrades a value from a json supported format
(: json-value->field-value (* symbol -> *))
(define (json-value->field-value value value-type)
  (cond ((or (not value) (eq? value 'json-null)) #f)
        ((eq? value-type 'date) (or (try-string->date value) 'invalid-value))
        ((eq? value-type 'date-time) (or (try-string->date-time value) 'invalid-value))
        ((eq? value-type 'time) (or (try-string->time value) 'invalid-value))
        (else value)))
