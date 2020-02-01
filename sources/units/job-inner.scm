(import srfi-4)

(import (chicken condition))
(import (chicken format))

(declare (unit job-inner))

;; invokes a procedure with a zmq-socket*
(: with-zmq-socket* (forall (r) (string fixnum ((struct zmq-socket*) -> r) -> r)))
(define (with-zmq-socket* endpoint type procedure)
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
