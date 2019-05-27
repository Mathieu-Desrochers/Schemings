(import (chicken base))
(import (chicken port))

(declare (unit debug))

;; prints a message to stderr
(: debug-print (string -> noreturn))
(define (debug-print message)
  (with-output-to-port
    (current-error-port)
    (lambda ()
      (print message))))
