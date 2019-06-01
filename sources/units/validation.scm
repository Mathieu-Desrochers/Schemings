(import (chicken blob))
(import (chicken condition))

(declare (unit validation))

(declare (uses date-time))
(declare (uses utf8))

;; validates a blob
(: validate-blob (* boolean -> (or symbol false)))
(define (validate-blob value required?)
  (cond ((eq? value 'invalid-value) 'invalid-value)
        ((not value) (if (not required?) #f 'missing))
        ((not (blob? value)) 'wrong-type)
        ((eq? (blob-size value) 0) (if (not required?) #f 'missing))
        (else #f)))

;; validates a boolean
(: validate-boolean (* -> (or symbol false)))
(define (validate-boolean value)
  (cond ((eq? value 'invalid-value) 'invalid-value)
        ((not (boolean? value)) 'wrong-type)
        (else #f)))

;; validates an integer
(: validate-integer (* boolean fixnum fixnum -> (or symbol false)))
(define (validate-integer value required? min-value max-value)
  (cond ((eq? value 'invalid-value) 'invalid-value)
        ((not value) (if (not required?) #f 'missing))
        ((not (and (integer? value) (exact? value))) 'wrong-type)
        ((< value min-value) 'too-low)
        ((> value max-value) 'too-high)
        (else #f)))

;; validates a number
(: validate-number (* boolean number number -> (or symbol false)))
(define (validate-number value required? min-value max-value)
  (cond ((eq? value 'invalid-value) 'invalid-value)
        ((not value) (if (not required?) #f 'missing))
        ((not (number? value)) 'wrong-type)
        ((< value min-value) 'too-low)
        ((> value max-value) 'too-high)
        (else #f)))

;; validates a string
(: validate-string (* boolean fixnum fixnum -> (or symbol false)))
(define (validate-string value required? min-length max-length)
  (cond ((eq? value 'invalid-value) 'invalid-value)
        ((not value) (if (not required?) #f 'missing))
        ((not (string? value)) 'wrong-type)
        ((< (utf8-length value) min-length) 'too-short)
        ((> (utf8-length value) max-length) 'too-long)
        (else #f)))

;; validates a date
(: validate-date (* boolean -> (or symbol false)))
(define (validate-date value required?)
  (cond ((not value) (if (not required?) #f 'missing))
        ((eq? value 'invalid-value) 'invalid-value)
        ((not (date? value)) 'wrong-type)
        (else #f)))

;; validates a date-time
(: validate-date-time (* boolean -> (or symbol false)))
(define (validate-date-time value required?)
  (cond ((eq? value 'invalid-value) 'invalid-value)
        ((not value) (if (not required?) #f 'missing))
        ((not (date-time? value)) 'wrong-type)
        (else #f)))

;; validates a time
(: validate-time (* boolean -> (or symbol false)))
(define (validate-time value required?)
  (cond ((eq? value 'invalid-value) 'invalid-value)
        ((not value) (if (not required?) #f 'missing))
        ((not (time? value)) 'wrong-type)
        (else #f)))

;; validates a list
(: validate-list (* boolean fixnum fixnum -> (or symbol false)))
(define (validate-list value required? min-length max-length)
  (cond ((eq? value 'invalid-value) 'invalid-value)
        ((not value) (if (not required?) #f 'missing))
        ((not (list? value)) 'wrong-type)
        ((< (length value) min-length) 'too-few)
        ((> (length value) max-length) 'too-many)
        (else #f)))

;; validates a record
(: validate-record (* boolean -> (or symbol false)))
(define (validate-record value required?)
  (cond ((eq? value 'invalid-value) 'invalid-value)
        ((not value) (if (not required?) #f 'missing))
        (else #f)))

;; raises a validation errors exception
(: raise-validation-errors-exception ((list-of symbol) -> noreturn))
(define (raise-validation-errors-exception validation-errors)
  (let ((condition (make-property-condition 'validation 'validation-errors validation-errors)))
    (abort condition)))

;; returns whether an exception was caused by validation errors
(: validation-errors-exception? (condition -> boolean))
(define (validation-errors-exception? exception)
  ((condition-predicate 'validation)
    exception))

;; returns the validation errors from an exception
(: validation-errors-exception-validation-errors (condition -> (list-of symbol)))
(define (validation-errors-exception-validation-errors exception)
  ((condition-property-accessor 'validation 'validation-errors)
    exception))
