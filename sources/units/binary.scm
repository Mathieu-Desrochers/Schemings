(import srfi-4)

(import (chicken blob))
(import (chicken condition))
(import (chicken format))

(declare (unit binary))

(declare (uses binary-inner))
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

;; adds a boolean value to a binary packer
(: binary-packer-add-boolean ((struct binary-packer) boolean -> noreturn))
(define (binary-packer-add-boolean binary-packer value)
  (if value
    (unless (eq? (msgpack-pack-true (binary-packer-msgpack-msgpack-packer* binary-packer)) 0)
      (abort "failed to pack true"))
    (unless (eq? (msgpack-pack-false (binary-packer-msgpack-msgpack-packer* binary-packer)) 0)
      (abort "failed to pack false"))))

;; adds an integer value to a binary packer
(: binary-packer-add-integer ((struct binary-packer) fixnum -> noreturn))
(define (binary-packer-add-integer binary-packer value)
  (unless (eq? (msgpack-pack-int (binary-packer-msgpack-msgpack-packer* binary-packer) value) 0)
    (abort
      (format "failed to pack integer ~A"
        value))))

;; adds a double value to a binary packer
(: binary-packer-add-double ((struct binary-packer) float -> noreturn))
(define (binary-packer-add-double binary-packer value)
  (unless (eq? (msgpack-pack-double (binary-packer-msgpack-msgpack-packer* binary-packer) value) 0)
    (abort
      (format "failed to pack double ~A"
        value))))

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

;; returns the data from a binary packer
(: binary-packer-data ((struct binary-packer) -> u8vector))
(define (binary-packer-data binary-packer)
  (blob->u8vector
    (string->blob
      (msgpack-sbuffer-data
        (binary-packer-msgpack-sbuffer* binary-packer)))))

;; encapsulates a binary unpacker
(define-typed-record binary-unpacker
  (msgpack-unpacker* (struct msgpack-unpacker*))
  (msgpack-unpacked* (struct msgpack-unpacked*)))

;; invokes a procedure with a binary unpacker
(: with-binary-unpacker (forall (r) (u8vector ((struct binary-unpacker) -> r) -> r)))
(define (with-binary-unpacker data procedure)
  (with-guaranteed-release
    (lambda ()
      (let ((msgpack-unpacker* (msgpack-unpacker-new (u8vector-length data))))
        (unless msgpack-unpacker*
          (abort
            (format "failed to create msgpack-unpacker with size ~A"
              (u8vector-length data))))
        msgpack-unpacker*))
    (lambda (msgpack-unpacker*)
      (with-guaranteed-release
        (lambda ()
          (msgpack-unpacker-set-data msgpack-unpacker* data (u8vector-length data))
          (let ((msgpack-unpacked* (msgpack-unpacked-new)))
            (unless msgpack-unpacked*
              (abort "failed to create msgpack-unpacked"))
            msgpack-unpacked*))
        (lambda (msgpack-unpacked*)
          (msgpack-unpacked-init msgpack-unpacked*)
          (procedure
            (make-binary-unpacker
              msgpack-unpacker*
              msgpack-unpacked*)))
        msgpack-unpacked-free))
    msgpack-unpacker-free))

;; returns an unpacked boolean
(: binary-unpacker-boolean ((struct binary-unpacker) -> boolean))
(define (binary-unpacker-boolean binary-unpacker)
  (let* ((msgpack-object* (binary-unpacker-next binary-unpacker))
         (msgpack-object-type (msgpack-object-type msgpack-object*)))
    (unless (eq? msgpack-object-type 1)
      (abort
        (format "failed to unpack boolean got ~A instead"
          msgpack-object-type)))
    (eq? 1
      (msgpack-object-boolean
        msgpack-object*))))

;; returns an unpacked integer
(: binary-unpacker-integer ((struct binary-unpacker) -> fixnum))
(define (binary-unpacker-integer binary-unpacker)
  (let* ((msgpack-object* (binary-unpacker-next binary-unpacker))
         (msgpack-object-type (msgpack-object-type msgpack-object*)))
    (unless (or (eq? msgpack-object-type 2) (eq? msgpack-object-type 3))
      (abort
        (format "failed to unpack integer got ~A instead"
          msgpack-object-type)))
    (msgpack-object-int
      msgpack-object*)))

;; returns an unpacked double
(: binary-unpacker-double ((struct binary-unpacker) -> number))
(define (binary-unpacker-double binary-unpacker)
  (let* ((msgpack-object* (binary-unpacker-next binary-unpacker))
         (msgpack-object-type (msgpack-object-type msgpack-object*)))
    (unless (or (eq? msgpack-object-type 2)
                (eq? msgpack-object-type 3)
                (eq? msgpack-object-type 4)
                (eq? msgpack-object-type 10))
      (abort
        (format "failed to unpack double got ~A instead"
          msgpack-object-type)))
    (msgpack-object-double
      msgpack-object*)))

;; returns an unpacked string
(: binary-unpacker-string ((struct binary-unpacker) -> string))
(define (binary-unpacker-string binary-unpacker)
  (let* ((msgpack-object* (binary-unpacker-next binary-unpacker))
         (msgpack-object-type (msgpack-object-type msgpack-object*)))
    (unless (eq? msgpack-object-type 5)
      (abort
        (format "failed to unpack string got ~A instead"
          msgpack-object-type)))
    (msgpack-object-string
      msgpack-object*)))
