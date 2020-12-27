(import (chicken time))

(declare (unit monitoring))

(declare (uses statsd-client))

(define monitoring-global-statsd-link* #f)

;; initializes the monitoring
(: monitoring-init (string fixnum -> noreturn))
(define (monitoring-init ip-address port-number)
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
    (let* ((start (current-milliseconds))
           (procedure-result (procedure))
           (duration (- (current-milliseconds) start)))
      (monitoring-timing procedure-name duration)
      procedure-result)
    (procedure)))

;; releases the monitoring
(: monitoring-release (-> noreturn))
(define (monitoring-release)
  (if monitoring-global-statsd-link*
    (statsd-finalize monitoring-global-statsd-link*))
  (set! monitoring-global-statsd-link* #f))
