(declare (unit http-client-intern))

(declare (uses curl))
(declare (uses exceptions))

;; invokes a procedure with a curl instance
(: with-curl* (forall (r) ((pointer -> r) -> r)))
(define (with-curl* procedure)
  (with-guaranteed-release
    curl-easy-init
    procedure
    curl-easy-cleanup))
