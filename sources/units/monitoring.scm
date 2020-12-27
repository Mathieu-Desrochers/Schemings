(import (chicken time))

(declare (unit monitoring))

(declare (uses statsd-client))

(define monitoring-global-statsd-link* #f)

;; initializes the monitoring
(: monitoring-initialize (string fixnum -> noreturn))
(define (monitoring-initialize ip-address port-number)
  (set! monitoring-global-statsd-link* (statsd-init ip-address port-number)))

;; signals an event
(: monitoring-signal (string -> noreturn))
(define (monitoring-signal event-name)
  (if monitoring-global-statsd-link*
    (statsd-inc monitoring-global-statsd-link* event-name 1)))

;; sets a monitored value
(: monitoring-set-value (string fixnum -> noreturn))
(define (monitoring-set-value name value)
  (if monitoring-global-statsd-link*
    (statsd-gauge monitoring-global-statsd-link* name value)))

;; reports a timed operation
(: monitoring-timing (string fixnum -> noreturn))
(define (monitoring-timing operation-name duration)
  (if monitoring-global-statsd-link*
    (statsd-timing monitoring-global-statsd-link* operation-name duration)))

;; invokes a procedure and reports its timing
(: with-monitoring-timing (forall (r) (string (-> r) -> r)))
(define (with-monitoring-timing procedure-name procedure)
  (if monitoring-global-statsd-link*
    (let ((start (current-milliseconds)))
      (procedure)
      (monitoring-timing
        procedure-name
        (- (current-milliseconds) start)))))
