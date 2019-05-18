(import (chicken condition))

(declare (unit fastcgi-intern))

(declare (uses exceptions))
(declare (uses fcgi))

;; encapsulates a fastcgi request
(define-typed-record fastcgi-request
  (in-fcgx-stream* pointer)
  (out-fcgx-stream* pointer)
  (err-fcgx-stream* pointer)
  (fcgx-paramarray pointer))

;; invokes a procedure with the pointers needed by fastcgi-request
(: with-fastcgi-request-pointers
  (forall (r) ((pointer pointer pointer pointer -> r) -> r)))
(define (with-fastcgi-request-pointers procedure)
  (with-guaranteed-release
    (lambda ()
      (let ((in-fcgx-stream** (malloc-fcgx-stream*))
            (out-fcgx-stream** (malloc-fcgx-stream*))
            (err-fcgx-stream** (malloc-fcgx-stream*))
            (fcgx-paramarray* (malloc-fcgx-paramarray)))
        (unless
          (and in-fcgx-stream** out-fcgx-stream** err-fcgx-stream**)
          (abort "failed to allocate fcgx-stream*"))
        (unless fcgx-paramarray*
          (abort "failed to allocate fcgx-paramarray"))
        (list
          in-fcgx-stream**
          out-fcgx-stream**
          err-fcgx-stream**
          fcgx-paramarray*)))
    (lambda (fastcgi-request-pointers)
      (procedure
        (list-ref fastcgi-request-pointers 0)
        (list-ref fastcgi-request-pointers 1)
        (list-ref fastcgi-request-pointers 2)
        (list-ref fastcgi-request-pointers 3)))
    (lambda (fastcgi-request-pointers)
      (free-fcgx-stream* (list-ref fastcgi-request-pointers 0))
      (free-fcgx-stream* (list-ref fastcgi-request-pointers 1))
      (free-fcgx-stream* (list-ref fastcgi-request-pointers 2))
      (free-fcgx-paramarray (list-ref fastcgi-request-pointers 3)))))
