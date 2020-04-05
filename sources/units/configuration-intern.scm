(declare (unit configuration-intern))

(declare (uses config))
(declare (uses date-time))

;; returns the value of a config-setting-t*
(: get-value-from-config-setting-t* (pointer -> *))
(define (get-value-from-config-setting-t* config-setting-t*)
  (let ((config-type (config-setting-type config-setting-t*)))
    (cond ((eq? config-type config-type-int) (config-setting-get-int config-setting-t*))
          ((eq? config-type config-type-float) (config-setting-get-float config-setting-t*))
          ((eq? config-type config-type-string) (config-setting-get-string config-setting-t*))
          ((eq? config-type config-type-bool) (not (eq? (config-setting-get-bool config-setting-t*) 0)))
          (else #f))))

;; upgrades a value from a config supported format
(: config-value->setting-value (* symbol -> *))
(define (config-value->setting-value value value-type)
  (cond ((not value) #f)
        ((eq? value-type 'date) (or (try-string->date value) 'invalid-value))
        ((eq? value-type 'date-time) (or (try-string->date-time value) 'invalid-value))
        ((eq? value-type 'time) (or (try-string->time value) 'invalid-value))
        (else value)))
