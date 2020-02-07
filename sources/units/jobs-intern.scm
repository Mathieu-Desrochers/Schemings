(import srfi-4)

(import (chicken condition))
(import (chicken format))

(declare (unit exceptions))
(declare (unit jobs-intern))

;; invokes a procedure with a zmq-socket*
(: with-zmq-socket* (forall (r) (fixnum ((struct zmq-socket*) -> r) -> r)))
(define (with-zmq-socket* type procedure)
  (with-guaranteed-release
    (lambda ()
      (let ((zmq-ctx* (zmq-ctx-new)))
        (unless zmq-ctx*
          (abort "failed to create zmq-ctx"))
        zmq-ctx*))
    (lambda (zmq-ctx*)
      (with-guaranteed-release
        (lambda ()
          (let ((zmq-socket* (zmq-socket zmq-ctx* type)))
            (unless zmq-socket*
              (abort
                (format "failed to create socket of type ~A"
                  type)))
            zmq-socket*))
        procedure
        zmq-close))
    zmq-ctx-destroy))

;; sends a job on a zmq-socket*
(: send-on-zmq-socket* ((struct zmq-socket*) u8vector fixnum fixnum -> noreturn))
(define (send-on-zmq-socket* zmq-socket* u8vector length flags)
  (let ((zmq-send-result (zmq-send zmq-socket* u8vector length flags)))
    (unless (eq? zmq-send-result length)
      (abort
        (format "failed to send job of size ~A"
          length)))))

;; invokes a procedure with a job received from a zmq-socket*
(: receive-on-zmq-socket* (forall (r) ((struct zmq-socket*) (u8vector fixnum -> r) -> r)))
(define (receive-on-zmq-socket* zmq-socket* procedure)
  (let* ((ten-megabytes 10000000)
         (buffer (make-u8vector ten-megabytes))
         (zmq-recv-result (zmq-recv zmq-socket* buffer ten-megabytes 0)))
    (unless (and (not (eq? zmq-recv-result -1)) (<= zmq-recv-result ten-megabytes))
      (abort
        (format "failed to receive job of maximum size ~A"
          ten-megabytes)))
    (procedure
      buffer
      zmq-recv-result)))
