(import (chicken condition))

(declare (unit exceptions))

;; hides any exception raised by a procedure
(: with-exception-hiding ((-> *) -> *))
(define (with-exception-hiding procedure)
  (handle-exceptions
    exception
    #f
    (procedure)))

;; invokes a procedure with the guarantee
;; the allocated resource will be released
(: with-guaranteed-release (forall (x y) ((-> x) (x -> y) (x -> *) -> y)))
(define (with-guaranteed-release allocation-procedure procedure release-procedure)
  (let ((allocated-resource (allocation-procedure)))
    (handle-exceptions
      exception
      (begin
        (release-procedure allocated-resource)
        (abort exception))
      (let ((procedure-result (procedure allocated-resource)))
        (release-procedure allocated-resource)
        procedure-result))))
