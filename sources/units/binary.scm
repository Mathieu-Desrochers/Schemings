(import srfi-4)

(import (chicken blob))
(import (chicken condition))
(import (chicken format))

(declare (unit binary))

(declare (uses exceptions))
(declare (uses msgpack))

;; encapsulates a binary packer
(define-typed-record binary-packer
  (msgpack-sbuffer* (struct msgpack-sbuffer*))
  (msgpack-msgpack-packer* (struct msgpack-msgpack-packer*)))

;; invokes a procedure with a binary packer
(: with-binary-packer (forall (r) (((struct binary-packer) -> r) -> r)))
(define (with-binary-packer procedure)
  (with-guaranteed-release
    (lambda ()
      (let ((msgpack-sbuffer* (msgpack-sbuffer-new)))
        (unless msgpack-sbuffer*
          (abort "failed to create msgpack-sbuffer"))
        msgpack-sbuffer*))
    (lambda (msgpack-sbuffer*)
      (msgpack-sbuffer-init msgpack-sbuffer*)
      (with-guaranteed-release
        (lambda ()
          (let ((msgpack-packer* (msgpack-packer-new msgpack-sbuffer*)))
            (unless msgpack-packer*
              (abort "failed to create msgpack-packer"))
            msgpack-packer*))
        (lambda (msgpack-packer*)
          (procedure
            (make-binary-packer
              msgpack-sbuffer*
              msgpack-packer*)))
        msgpack-packer-free))
    msgpack-sbuffer-free))

;; adds an integer value to a binary packer
(: binary-packer-add-int ((struct binary-packer) fixnum -> noreturn))
(define (binary-packer-add-int binary-packer value)
  (unless (eq? (msgpack-pack-int (binary-packer-msgpack-msgpack-packer* binary-packer) value) 0)
    (abort
      (format "failed to pack int ~A"
        value))))

;; adds a double value to a binary packer
(: binary-packer-add-double ((struct binary-packer) float -> noreturn))
(define (binary-packer-add-double binary-packer value)
  (unless (eq? (msgpack-pack-double (binary-packer-msgpack-msgpack-packer* binary-packer) value) 0)
    (abort
      (format "failed to pack double ~A"
        value))))

;; adds a null value to a binary packer
(: binary-packer-add-null ((struct binary-packer) -> noreturn))
(define (binary-packer-add-null binary-packer)
  (unless (eq? (msgpack-pack-nil (binary-packer-msgpack-msgpack-packer* binary-packer)) 0)
    (abort "failed to pack null")))

;; adds a boolean value to a binary packer
(: binary-packer-add-boolean ((struct binary-packer) boolean -> noreturn))
(define (binary-packer-add-boolean binary-packer value)
  (if value
    (unless (eq? (msgpack-pack-true (binary-packer-msgpack-msgpack-packer* binary-packer)) 0)
      (abort "failed to pack true"))
    (unless (eq? (msgpack-pack-false (binary-packer-msgpack-msgpack-packer* binary-packer)) 0)
      (abort "failed to pack false"))))

;; adds an array value to a binary packer
(: binary-packer-add-array ((struct binary-packer) fixnum -> noreturn))
(define (binary-packer-add-array binary-packer size)
  (unless (eq? (msgpack-pack-array (binary-packer-msgpack-msgpack-packer* binary-packer) size) 0)
    (abort
      (format "failed to pack array of size ~A"
        size))))

;; adds a string value to a binary packer
(: binary-packer-add-string ((struct binary-packer) string -> noreturn))
(define (binary-packer-add-string binary-packer value)
  (unless
    (eq? 0
      (msgpack-pack-str
        (binary-packer-msgpack-msgpack-packer* binary-packer)
        (string-length value)))
    (abort
      (format "failed to pack string of length ~A"
        (string-length value))))
  (unless
    (eq? 0
      (msgpack-pack-str-body
        (binary-packer-msgpack-msgpack-packer* binary-packer)
        value
        (string-length value)))
    (abort
      (format "failed to pack string ~A"
        value))))

;; returns the size of a binary packer
(: binary-packer-size ((struct binary-packer) -> fixnum))
(define (binary-packer-size binary-packer)
  (msgpack-sbuffer-size
    (binary-packer-msgpack-sbuffer* binary-packer)))

;; returns the data of a binary packer
(: binary-packer-data ((struct binary-packer) -> u8vector))
(define (binary-packer-data binary-packer)
  (blob->u8vector
    (string->blob
      (msgpack-sbuffer-data
        (binary-packer-msgpack-sbuffer* binary-packer)))))
