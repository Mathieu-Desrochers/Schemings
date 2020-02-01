(import srfi-4)

(import (chicken blob))
(import (chicken condition))
(import (chicken format))

(declare (unit job-queue))

(declare (uses debug))
(declare (uses exceptions))
(declare (uses job-inner))
(declare (uses zeromq))

;; encapsulates a job queue connection
(define-typed-record job-queue-connection
  (zmq-socket* pointer))

;; starts a job queue
(: job-queue-start (string -> noreturn))
(define (job-queue-start endpoint)
  (with-zmq-socket*
    endpoint
    zmq-pull
    (lambda (zmq-socket*)
      (let ((zmq-bind-result (zmq-bind zmq-socket* endpoint)))
        (unless (eq? zmq-bind-result 0)
          (abort
            (format "failed to bind socket to endpoint ~A"
              endpoint)))
        (letrec* (
            (ten-megabytes 10000000)
            (buffer (make-u8vector ten-megabytes))
            (loop-inner
              (lambda ()
                (let ((zmq-recv-result (zmq-recv zmq-socket* buffer ten-megabytes 0)))
                  (unless (and (not (eq? zmq-recv-result -1)) (<= zmq-recv-result ten-megabytes))
                    (abort
                      (format "failed to receive job of maximum size ~A"
                        ten-megabytes)))
                  (debug-print
                    (blob->string
                      (u8vector->blob
                        (subu8vector buffer 0 zmq-recv-result))))
                  (loop-inner)))))
          (loop-inner))))))

;; invokes a procedure with a job queue connection
(: with-job-queue-connection (forall (r) (string ((struct job-queue-connection) -> r) -> r)))
(define (with-job-queue-connection endpoint procedure)
  (with-zmq-socket*
    endpoint
    zmq-push
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

;; sends a binary to a job queue connection
(: job-queue-send ((struct job-queue-connection) u8vector -> noreturn))
(define (job-queue-send job-queue-connection u8vector)
  (let ((zmq-send-result
          (zmq-send
              (job-queue-connection-zmq-socket* job-queue-connection)
              u8vector
              (u8vector-length u8vector)
              zmq-dontwait)))
    (unless (eq? zmq-send-result (u8vector-length u8vector))
      (abort
        (format "failed to send job of size ~A"
          (u8vector-length u8vector))))))
