(import srfi-4)

(import (chicken condition))
(import (chicken format))

(declare (unit job-queue))

(declare (uses exceptions))
(declare (uses job-inner))
(declare (uses zeromq))

;; encapsulates a job queue connection
(define-typed-record job-queue-connection
  (zmq-socket* (struct zmq-socket*)))

;; starts a job queue
(: job-queue-start (string string -> noreturn))
(define (job-queue-start submit-endpoint worker-endpoint)

  ;; bind the submit socket
  (with-zmq-socket* submit-endpoint zmq-pull
    (lambda (submit-zmq-socket*)
      (let ((zmq-bind-result (zmq-bind submit-zmq-socket* submit-endpoint)))
        (unless (eq? zmq-bind-result 0)
          (abort
            (format "failed to bind submit socket to endpoint ~A"
              submit-endpoint)))

        ;; bind the worker socket
        (with-zmq-socket* worker-endpoint zmq-push
          (lambda (worker-zmq-socket*)
            (let ((zmq-bind-result (zmq-bind worker-zmq-socket* worker-endpoint)))
              (unless (eq? zmq-bind-result 0)
                (abort
                  (format "failed to bind worker socket to endpoint ~A"
                    worker-endpoint)))

              ;; receive jobs and send them to a worker
              (letrec (
                  (loop-inner
                    (lambda ()
                      (with-job-received submit-zmq-socket*
                        (lambda (u8vector length)
                          (zmq-send worker-zmq-socket* u8vector length zmq-dontwait)
                          (loop-inner))))))

                (loop-inner)))))))))

;; invokes a procedure with a job queue connection
(: with-job-queue-connection (forall (r) (string ((struct job-queue-connection) -> r) -> r)))
(define (with-job-queue-connection endpoint procedure)
  (with-zmq-socket* endpoint zmq-push
    (lambda (zmq-socket*)
      (zmq-setsockopt-int zmq-socket* zmq-sndtimeo 0)
      (zmq-setsockopt-int zmq-socket* zmq-linger 0)
      (let ((zmq-connect-result (zmq-connect zmq-socket* endpoint)))
        (unless (eq? zmq-connect-result 0)
          (abort
            (format "failed to connect socket to endpoint ~A"
              endpoint)))
        (procedure
          (make-job-queue-connection
            zmq-socket*))))))

;; sends a job to a queue
(: job-queue-send ((struct job-queue-connection) u8vector -> noreturn))
(define (job-queue-send job-queue-connection u8vector)
  (zmq-send
    (job-queue-connection-zmq-socket* job-queue-connection)
    u8vector
    (u8vector-length u8vector)
    zmq-dontwait))

;; invokes a procedure with jobs received from a queue
(: job-queue-worker (forall (r) (string (u8vector fixnum -> r) -> r)))
(define (job-queue-worker worker-endpoint procedure)
  (with-zmq-socket* worker-endpoint zmq-pull
    (lambda (zmq-socket*)
      (with-job-received
        zmq-socket*
        procedure))))
