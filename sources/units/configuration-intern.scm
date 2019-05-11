(declare (unit configuration-intern))

(declare (uses config))

;; returns the value of a config-setting-t*
(: get-value-from-config-setting-t* (pointer -> *))
(define (get-value-from-config-setting-t* config-setting-t*)
  (let ((config-type (config-setting-type config-setting-t*)))
    (cond ((eq? config-type config-type-int) (config-setting-get-int config-setting-t*))
          ((eq? config-type config-type-float) (config-setting-get-float config-setting-t*))
          ((eq? config-type config-type-string) (config-setting-get-string config-setting-t*))
          ((eq? config-type config-type-bool) (not (eq? (config-setting-get-bool config-setting-t*) 0)))
          (else #f))))
