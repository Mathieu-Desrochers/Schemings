(import srfi-4)

(import (chicken condition))
(import (chicken format))

(declare (unit jobs-queue))

(declare (uses exceptions))
(declare (uses jobs-queue-intern))
(declare (uses zeromq))

;; encapsulates a jobs queue connection
(define-typed-record jobs-queue-connection
  (zmq-socket* (struct zmq-socket*)))

;; starts a jobs queue
(: jobs-queue-start (string string -> noreturn))
(define (jobs-queue-start submit-endpoint worker-endpoint)
  (with-zmq-socket* zmq-pull
    (lambda (submit-zmq-socket*)
      (let ((zmq-bind-result (zmq-bind submit-zmq-socket* submit-endpoint)))
        (unless (eq? zmq-bind-result 0)
          (abort
            (format "failed to bind submit socket to endpoint ~A"
              submit-endpoint)))
        (with-zmq-socket* zmq-push
          (lambda (worker-zmq-socket*)
            (let ((zmq-bind-result (zmq-bind worker-zmq-socket* worker-endpoint)))
              (unless (eq? zmq-bind-result 0)
                (abort
                  (format "failed to bind worker socket to endpoint ~A"
                    worker-endpoint)))
              (letrec (
                  (loop-inner
                    (lambda ()
                      (receive-on-zmq-socket* submit-zmq-socket*
                        (lambda (u8vector length)
                          (send-on-zmq-socket* worker-zmq-socket* u8vector length 0)
                          (loop-inner))))))
                (loop-inner)))))))))

;; invokes a procedure with a jobs queue connection
(: with-jobs-queue-connection (forall (r) (string ((struct jobs-queue-connection) -> r) -> r)))
(define (with-jobs-queue-connection submit-endpoint procedure)
  (with-zmq-socket* zmq-push
    (lambda (zmq-socket*)
      (zmq-setsockopt-int zmq-socket* zmq-sndtimeo 0)
      (zmq-setsockopt-int zmq-socket* zmq-linger 0)
      (let ((zmq-connect-result (zmq-connect zmq-socket* submit-endpoint)))
        (unless (eq? zmq-connect-result 0)
          (abort
            (format "failed to connect socket to endpoint ~A"
              submit-endpoint)))
        (procedure
          (make-jobs-queue-connection
            zmq-socket*))))))

;; submits a job to a queue
(: jobs-queue-submit ((struct jobs-queue-connection) u8vector -> noreturn))
(define (jobs-queue-submit jobs-queue-connection u8vector)
  (send-on-zmq-socket*
    (jobs-queue-connection-zmq-socket* jobs-queue-connection)
    u8vector
    (u8vector-length u8vector)
    zmq-dontwait))

;; invokes a procedure with jobs received from a queue
(: jobs-queue-receive (forall (r) (string (u8vector -> r) -> r)))
(define (jobs-queue-receive worker-endpoint procedure)
  (with-zmq-socket* zmq-pull
    (lambda (zmq-socket*)
      (let ((zmq-connect-result (zmq-connect zmq-socket* worker-endpoint)))
        (unless (eq? zmq-connect-result 0)
          (abort
            (format "failed to connect socket to endpoint ~A"
              worker-endpoint)))
        (letrec* (
            (procedure-wrapper
              (lambda (u8vector length)
                (procedure (subu8vector u8vector 0 length))))
            (loop-inner
              (lambda ()
                (receive-on-zmq-socket* zmq-socket* procedure-wrapper)
                (loop-inner))))
          (loop-inner))))))
