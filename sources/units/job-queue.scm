(import srfi-4)

(import (chicken condition))
(import (chicken format))

(declare (unit job-queue))

(declare (uses debug))
(declare (uses exceptions))
(declare (uses zeromq))

;; encapsulates a job queue
(define-typed-record job-queue
  (zmq-ctx* pointer)
  (zmq-socket* pointer))

;; invokes a procedure with a job queue
(: with-job-queue (forall (r) (string ((struct job-queue) -> r) -> r)))
(define (with-job-queue endpoint procedure)
  (with-guaranteed-release
    (lambda ()
      (let ((zmq-ctx* (zmq-ctx-new)))
        (unless zmq-ctx*
          (abort "failed to create zmq-ctx"))
        zmq-ctx*))
    (lambda (zmq-ctx*)
      (with-guaranteed-release
        (lambda ()
          (let ((zmq-socket* (zmq-socket zmq-ctx* zmq-push)))
            (unless zmq-socket*
              (abort "failed to create zmq-socket"))
            zmq-socket*))
        (lambda (zmq-socket*)
          (zmq-setsockopt-int zmq-socket* zmq-sndtimeo 0)
          (zmq-setsockopt-int zmq-socket* zmq-linger 0)
          (let ((zmq-connect-result (zmq-connect zmq-socket* endpoint)))
            (unless (eq? zmq-connect-result 0)
              (abort
                (format "failed to connect socket to endpoint ~A"
                  endpoint)))
            (procedure
              (make-job-queue
                zmq-ctx*
                zmq-socket*))))
        (lambda (zmq-socket*)
          (zmq-close zmq-socket*))))
    zmq-ctx-destroy))

;; sends a binary job to a queue
(: job-queue-send ((struct job-queue) u8vector -> noreturn))
(define (job-queue-send job-queue u8vector)
  (let ((zmq-send-result
          (zmq-send
              (job-queue-zmq-socket* job-queue)
              u8vector
              (u8vector-length u8vector)
              zmq-dontwait)))
    (unless (eq? zmq-send-result (u8vector-length u8vector))
      (abort
        (format "failed to send job of size ~A"
          (u8vector-length u8vector))))))
