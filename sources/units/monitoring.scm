(declare (unit monitoring))

(declare (uses exceptions))
(declare (uses statsd-client))

;; encapsulates a monitor
(define-typed-record monitor
  (statsd-link* pointer))

;; invokes a procedure with monitoring
(: with-monitoring (forall (r) (string fixnum ((struct monitor) -> r) -> r)))
(define (with-monitoring ip-address port procedure)
  (with-guaranteed-release
    (lambda ()
      (statsd-init ip-address port))
    (lambda (statsd-link*)
      (procedure (make-monitor statsd-link*)))
    statsd-finalize))

;; signals an event
(define (monitoring-event monitor name)
  (statsd-inc (monitor-statsd-link* monitor) name 1))

;; sets a gauge value
(define (monitoring-gauge-set monitor name value)
  (statsd-gauge (monitor-statsd-link* monitor) name value))

;; records a timing
(define (monitoring-timing monitor name duration)
  (statsd-timing (monitor-statsd-link* monitor) name duration))
