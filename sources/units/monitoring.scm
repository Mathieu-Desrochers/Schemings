(import (chicken time))

(declare (unit monitoring))

(declare (uses exceptions))
(declare (uses statsd-client))

;; encapsulates a monitor
(define-typed-record monitor
  (statsd-link* pointer))

;; invokes a procedure with monitoring
(: with-monitoring (forall (r) (string fixnum ((struct monitor) -> r) -> r)))
(define (with-monitoring ip-address port-number procedure)
  (with-guaranteed-release
    (lambda ()
      (statsd-init ip-address port-number))
    (lambda (statsd-link*)
      (procedure (make-monitor statsd-link*)))
    statsd-finalize))

;; signals an event
(: monitoring-signal ((struct monitor) string -> noreturn))
(define (monitoring-signal monitor event-name)
  (statsd-inc (monitor-statsd-link* monitor) event-name 1))

;; sets a monitored value
(: monitoring-set-value ((struct monitor) string fixnum -> noreturn))
(define (monitoring-set-value monitor name value)
  (statsd-gauge (monitor-statsd-link* monitor) name value))

;; reports a timed operation
(: monitoring-timing ((struct monitor) string fixnum -> noreturn))
(define (monitoring-timing monitor operation-name duration)
  (statsd-timing (monitor-statsd-link* monitor) operation-name duration))

;; invokes a procedure and reports its timing
(: with-monitoring-timing (forall (r) ((struct monitor) string (-> r) -> r)))
(define (with-monitoring-timing monitor procedure-name procedure)
  (let ((start (current-milliseconds)))
    (procedure)
    (monitoring-timing monitor procedure-name
      (- (current-milliseconds) start))))
